#!/usr/bin/env python3
"""squire ai env info — Detect sandbox/network restrictions in the current environment.

Usage:
  python3 ai-env-info.py                    # print structured JSON to stdout
  python3 ai-env-info.py --check jvm-network # exit 0 if OK, 1 if blocked (no stdout)
  python3 ai-env-info.py --check var-folders
  python3 ai-env-info.py --check python-network
  python3 ai-env-info.py --timeout 8         # bound the live JVM probe (default 8s)

Other skills and scripts (mill task wrappers, cellar helpers, etc.) can shell out to
this script to make a data-driven decision instead of guessing from CLAUDE_CODE_*
env vars alone — presence of those vars means "running under Claude Code", NOT
"sandboxed". Whether a given session is actually restricted is controlled by the
`sandbox` key in Claude settings (managed/user/project/project-local, see
references/env.md) and varies per session/config, so this script combines that
static config (best-effort, informational) with a live functional probe (the
authoritative signal — see check-mill-daemon.py's note that Python socket success
does not guarantee JVM java.net.Socket success).

Design notes:
  - No caching: the JVM probe is a fresh, self-contained process (~1-2s normally,
    bounded to --timeout on failure) that exits cleanly and leaves nothing running,
    unlike starting a real mill daemon. Cheap enough to run on every invocation.
  - `sandboxed` (top-level bool) mirrors checks.jvm_network.ok being False, since
    that's the check most consumers (mill wrappers) care about. Consumers with a
    different concern (e.g. cellar caring about /var/folders) should read the
    specific check in `checks` rather than relying on the top-level bool.
"""

import argparse
import json
import os
import pathlib
import shutil
import socket
import subprocess
import sys
import tempfile
import time


CLAUDE_ENV_VARS = (
    "CLAUDECODE",
    "CLAUDE_CODE_ENTRYPOINT",
    "CLAUDE_CODE_SESSION_ID",
    "CLAUDE_CODE_CHILD_SESSION",
)

MANAGED_SETTINGS_CANDIDATES = (
    pathlib.Path("/Library/Application Support/ClaudeCode/managed-settings.json"),  # macOS
    pathlib.Path("/etc/claude-code/managed-settings.json"),                          # Linux
)

JAVA_PROBE_SOURCE = """
import java.io.*;
import java.net.*;

class Probe {
    public static void main(String[] args) throws Exception {
        ServerSocket server = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"));
        int port = server.getLocalPort();
        Socket client = new Socket();
        client.connect(new InetSocketAddress("127.0.0.1", port), 2000);
        Socket accepted = server.accept();
        client.close();
        accepted.close();
        server.close();
        System.out.println("OK");
    }
}
"""


def is_claude_code():
    return any(os.environ.get(v) for v in CLAUDE_ENV_VARS)


def claude_code_info():
    return {
        "detected": is_claude_code(),
        "entrypoint": os.environ.get("CLAUDE_CODE_ENTRYPOINT"),
        "session_id": os.environ.get("CLAUDE_CODE_SESSION_ID"),
        "child_session": os.environ.get("CLAUDE_CODE_CHILD_SESSION") == "1",
    }


def is_ci():
    return bool(os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS"))


def load_json(path):
    try:
        if path.exists():
            return json.loads(path.read_text())
    except (OSError, ValueError):
        pass
    return None


def read_claude_settings():
    """Best-effort read of the sandbox settings across all levels Claude Code merges.

    This is informational context, not authoritative — see module docstring. A
    missing `sandbox` key does not mean unsandboxed; it may mean the default
    (which is not always visible from files alone) applies.
    """
    sources = {}

    managed_path = next((p for p in MANAGED_SETTINGS_CANDIDATES if p.exists()), None)
    sources["managed"] = {"path": str(managed_path) if managed_path else None,
                           "settings": load_json(managed_path) if managed_path else None}

    home = pathlib.Path.home()
    sources["user"] = {"path": str(home / ".claude" / "settings.json"),
                        "settings": load_json(home / ".claude" / "settings.json")}

    sources["project"] = {"path": str(pathlib.Path(".claude/settings.json")),
                           "settings": load_json(pathlib.Path(".claude/settings.json"))}

    sources["project_local"] = {"path": str(pathlib.Path(".claude/settings.local.json")),
                                 "settings": load_json(pathlib.Path(".claude/settings.local.json"))}

    sandbox_enabled = {}
    allowed_domains = []
    denied_domains = []
    for name, entry in sources.items():
        settings = entry["settings"] or {}
        sandbox = settings.get("sandbox") or {}
        sandbox_enabled[name] = sandbox.get("enabled")
        network = sandbox.get("network") or {}
        allowed_domains.extend(network.get("allowedDomains") or [])
        denied_domains.extend(network.get("deniedDomains") or [])
        entry.pop("settings", None)  # keep output small; only report the path + whether it existed
        entry["present"] = settings is not None and bool(settings)

    return {
        "sources": sources,
        "sandbox_enabled": sandbox_enabled,
        "network_allowed_domains": sorted(set(allowed_domains)),
        "network_denied_domains": sorted(set(denied_domains)),
    }


def check_python_network(timeout):
    start = time.monotonic()
    try:
        server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server.bind(("127.0.0.1", 0))
        server.listen(1)
        server.settimeout(timeout)
        port = server.getsockname()[1]

        client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        client.settimeout(timeout)
        client.connect(("127.0.0.1", port))
        accepted, _ = server.accept()
        client.close()
        accepted.close()
        server.close()
        return {"ok": True, "detail": "loopback bind+accept+connect succeeded", "duration_s": round(time.monotonic() - start, 3)}
    except OSError as e:
        return {"ok": False, "detail": f"{type(e).__name__}: {e}", "duration_s": round(time.monotonic() - start, 3)}


def check_jvm_network(timeout):
    java = shutil.which("java")
    if not java:
        return {"ok": None, "detail": "java not found on PATH — check skipped", "duration_s": 0.0}

    start = time.monotonic()
    with tempfile.TemporaryDirectory() as tmp:
        src = pathlib.Path(tmp) / "Probe.java"
        src.write_text(JAVA_PROBE_SOURCE)
        try:
            result = subprocess.run(
                ["timeout", str(timeout), java, str(src)],
                capture_output=True, text=True
            )
        except FileNotFoundError:
            # `timeout` command unavailable (rare outside Linux/macOS+coreutils); run unbounded.
            result = subprocess.run([java, str(src)], capture_output=True, text=True, timeout=timeout)

        duration = round(time.monotonic() - start, 3)
        if result.returncode == 124:
            return {"ok": False, "detail": f"java probe hung past {timeout}s timeout — JVM sockets likely blocked by sandbox", "duration_s": duration}
        if result.returncode == 0 and "OK" in result.stdout:
            return {"ok": True, "detail": "loopback bind+accept+connect succeeded", "duration_s": duration}
        return {"ok": False, "detail": (result.stderr or result.stdout or "non-zero exit").strip()[:300], "duration_s": duration}


def check_var_folders():
    probe_dir = pathlib.Path("/var/folders")
    if not probe_dir.exists():
        return {"ok": None, "detail": "/var/folders does not exist on this platform — check skipped"}
    probe_path = probe_dir / ".squire-env-probe"
    try:
        probe_path.write_text("squire probe")
        probe_path.unlink()
        return {"ok": True, "detail": "write probe succeeded"}
    except OSError as e:
        return {"ok": False, "detail": f"{type(e).__name__}: {e}"}


CHECKS = {
    "jvm-network": "jvm_network",
    "var-folders": "var_folders_writable",
    "python-network": "python_network",
}


def build_report(timeout):
    checks = {
        "python_network": check_python_network(timeout),
        "jvm_network": check_jvm_network(timeout),
        "var_folders_writable": check_var_folders(),
    }
    return {
        "generated_at": time.strftime("%Y-%m-%dT%H:%M:%S%z"),
        "claude_code": claude_code_info(),
        "ci": is_ci(),
        "checks": checks,
        "sandboxed": checks["jvm_network"]["ok"] is False,
        "claude_settings": read_claude_settings(),
        "recommendation": {
            "mill_daemon": "ok" if checks["jvm_network"]["ok"] is not False else "use_no_server",
        },
    }


def main():
    parser = argparse.ArgumentParser(description="Detect sandbox/network restrictions for AI agent environments")
    parser.add_argument("--check", choices=sorted(CHECKS.keys()),
                        help="Run a single check and exit 0 (ok) / 1 (blocked) instead of printing JSON")
    parser.add_argument("--timeout", type=float, default=8.0,
                        help="Seconds to bound the live JVM/socket probes (default: 8)")
    args = parser.parse_args()

    if args.check:
        key = CHECKS[args.check]
        if key == "jvm_network":
            result = check_jvm_network(args.timeout)
        elif key == "python_network":
            result = check_python_network(args.timeout)
        else:
            result = check_var_folders()
        sys.exit(0 if result["ok"] is not False else 1)

    print(json.dumps(build_report(args.timeout), indent=2))


if __name__ == "__main__":
    main()
