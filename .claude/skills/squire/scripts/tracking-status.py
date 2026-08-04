#!/usr/bin/env python3
"""
squire tracking status — Report the effective task-tracking mode for this checkout.

Resolves whether beads (`bd`) should be used in this working copy, from three inputs:
the contributor's opt-out setting, whether `bd` is on PATH, and whether `.beads/` exists.
Agents and scripts should read `effective_mode` rather than assuming beads is in play.

Usage:
  python3 tracking-status.py                 # full JSON report
  python3 tracking-status.py --quiet         # one line: the effective mode
  python3 tracking-status.py --check beads   # exit 0 if effective_mode == beads, else 1

Configuration lives in .config/squire/settings.local.yaml (gitignored):

  tracking:
    mode: auto | beads | off

See docs/task-tracking.md for what each mode means.
"""

import argparse
import json
import pathlib
import shutil
import subprocess
import sys

SETTINGS_FILE = pathlib.Path(".config/squire/settings.local.yaml")
BEADS_DIR = pathlib.Path(".beads")
GUIDANCE_DOC = pathlib.Path("docs/task-tracking.md")

VALID_MODES = ("auto", "beads", "off")


def load_local_settings():
    """Load .config/squire/settings.local.yaml if it exists. Returns a dict."""
    if not SETTINGS_FILE.exists():
        return {}
    try:
        import importlib.util
        if importlib.util.find_spec("yaml") is not None:
            import yaml
            return yaml.safe_load(SETTINGS_FILE.read_text()) or {}
        # Minimal fallback: find `mode:` nested under `tracking:`. Enough for this
        # one scalar; anything richer needs PyYAML.
        mode, in_tracking = None, False
        for line in SETTINGS_FILE.read_text().splitlines():
            if not line.strip() or line.strip().startswith("#"):
                continue
            if not line.startswith((" ", "\t")):
                in_tracking = line.strip().startswith("tracking:")
                continue
            if in_tracking and line.strip().startswith("mode:"):
                mode = line.split(":", 1)[1].strip().strip("\"'")
        return {"tracking": {"mode": mode}} if mode else {}
    except Exception as e:
        print(f"Warning: could not parse {SETTINGS_FILE}: {e}", file=sys.stderr)
        return {}


def configured_mode(settings):
    """The mode as written by the contributor, plus any complaint about it."""
    raw = (settings.get("tracking") or {}).get("mode")
    if raw is None:
        return "auto", None
    # YAML parses a bare `off` as boolean False, and `on` as True. Both are the
    # spellings a human would reach for, so accept them rather than rejecting.
    if raw is False:
        return "off", None
    if raw is True:
        return "beads", None
    mode = str(raw).strip().lower()
    if mode not in VALID_MODES:
        return "auto", f"unrecognised tracking.mode {raw!r}; expected one of {', '.join(VALID_MODES)}"
    return mode, None


def bd_version():
    if shutil.which("bd") is None:
        return None
    try:
        out = subprocess.run(["bd", "version"], capture_output=True, text=True, timeout=10)
        return (out.stdout or out.stderr).strip().splitlines()[0] if out.stdout or out.stderr else None
    except Exception:
        return None


def guidance_drift():
    """Detect bd re-adding its own long guidance blocks to AGENTS.md / CLAUDE.md.

    The repo owns a short pointer block instead; `bd init` and `bd setup <editor>`
    both re-append their own. Reported so `/squire tracking sync` can fix it.
    """
    drift = []
    for name in ("AGENTS.md", "CLAUDE.md"):
        path = pathlib.Path(name)
        if not path.exists():
            continue
        text = path.read_text()
        has_bd_block = "BEGIN BEADS INTEGRATION" in text or "BEGIN BEADS CODEX SETUP" in text
        has_pointer = "BEGIN MORPHIR TRACKING" in text
        if has_bd_block:
            drift.append({"file": name, "issue": "bd-managed guidance block present; expected the repo pointer"})
        elif not has_pointer:
            drift.append({"file": name, "issue": "no tracking pointer block found"})
    return drift


def git_dirs():
    """`(git_dir, common_dir)` for this checkout, or `(None, None)` outside a repository.

    They differ exactly when the current working copy is a git worktree.
    """
    try:
        out = subprocess.run(
            ["git", "rev-parse", "--git-dir", "--git-common-dir"],
            capture_output=True, text=True, timeout=10,
        )
        lines = out.stdout.strip().splitlines()
        if out.returncode != 0 or len(lines) < 2:
            return None, None
        return pathlib.Path(lines[0]).resolve(), pathlib.Path(lines[1]).resolve()
    except Exception:
        return None, None


def workspace_resolution():
    """Where `bd` will look for the beads workspace, and whether it will find a usable one.

    `.beads/` existing is not the same as bd resolving to a workspace. bd walks up from the
    working directory for `.beads/config.yaml` and, finding none, falls back to the *main
    clone's* `.beads/` — see `worktreeFallbackConfigPath` in beads' internal/config/config.go.
    That is the right default: one issue database per repository, not one per worktree.

    The catch is that the fallback targets a **tracked** file. In a worktree it therefore
    resolves against whichever branch the main clone happens to have checked out, which is not
    this worktree's branch. If that branch predates the repo adopting beads, bd finds no config,
    silently defaults the database name, and reports an empty workspace — while `.beads/` sits
    right here, fully populated, on this branch.

    That is how a checkout can report `effective_mode: beads` and still fail every `bd create`
    with "issue_prefix config is missing". Detecting it is cheaper than diagnosing it twice.
    """
    git_dir, common_dir = git_dirs()
    if git_dir is None:
        return {"status": "no-repo", "detail": "not inside a git repository"}

    is_worktree = git_dir != common_dir
    # The data store, as opposed to the tracked config. Any of these means this working copy
    # has a workspace of its own and bd will not fall back.
    local_store = any((BEADS_DIR / name).exists() for name in ("embeddeddolt", "dolt", "proxieddb")) \
        or any(BEADS_DIR.glob("*.db"))

    info = {"is_worktree": is_worktree, "local_store": local_store}

    if not is_worktree or local_store:
        info["status"] = "local" if local_store else "shared"
        return info

    # A worktree with no store of its own: bd will use the main clone's config.
    fallback = common_dir.parent / ".beads" / "config.yaml" if common_dir.name == ".git" \
        else common_dir / ".beads" / "config.yaml"
    info["fallback_config"] = str(fallback)
    if fallback.exists():
        info["status"] = "shared"
        return info

    info["status"] = "unresolvable"
    info["detail"] = (
        f"this is a git worktree with no beads workspace of its own, and the main clone's "
        f"{fallback} does not exist — most likely it is on a branch that predates beads. "
        f"bd will silently default the database name and report an empty workspace."
    )
    info["remedy"] = "run `bd bootstrap` here to clone the workspace from the remote ref"
    return info


def resolve():
    settings = load_local_settings()
    configured, complaint = configured_mode(settings)
    version = bd_version()
    installed = version is not None
    initialized = BEADS_DIR.exists()
    workspace = workspace_resolution()

    if configured == "off":
        effective, why = "off", "contributor opted out via tracking.mode: off"
    elif not installed:
        effective, why = "unavailable", "bd is not on PATH"
    elif not initialized:
        effective, why = "unavailable", f"{BEADS_DIR} does not exist in this checkout"
    elif workspace.get("status") == "unresolvable":
        # `.beads/` is present but bd will not resolve to it. Reporting `beads` here would send
        # an agent off to run commands that cannot work, which is the failure this check exists
        # to prevent.
        effective, why = "unavailable", workspace["detail"]
    else:
        effective, why = "beads", (
            "tracking.mode: beads" if configured == "beads"
            else "bd is installed and .beads/ exists (mode: auto)"
        )

    report = {
        "configured_mode": configured,
        "effective_mode": effective,
        "reason": why,
        "bd": {"installed": installed, "version": version},
        "beads_dir_present": initialized,
        "workspace": workspace,
        "settings_file": str(SETTINGS_FILE),
        "settings_file_present": SETTINGS_FILE.exists(),
        "guidance_doc": str(GUIDANCE_DOC),
        "guidance_drift": guidance_drift(),
    }
    if complaint:
        report["warning"] = complaint
    if configured == "beads" and effective == "unavailable":
        report["warning"] = (
            f"tracking.mode is 'beads' but {why}. Install bd, or set mode to auto/off. "
            "See docs/task-tracking.md."
        )
    return report


def main():
    ap = argparse.ArgumentParser(add_help=True, description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--quiet", "-q", action="store_true", help="print only the effective mode")
    ap.add_argument("--check", metavar="MODE", choices=VALID_MODES + ("unavailable",),
                    help="exit 0 if effective_mode matches, 1 otherwise")
    args = ap.parse_args()

    report = resolve()

    if args.check:
        sys.exit(0 if report["effective_mode"] == args.check else 1)
    if args.quiet:
        print(report["effective_mode"])
        return
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
