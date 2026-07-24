---
name: squire
description: "Development environment diagnostics and unblocking for the morphir-scala project. Use when hitting build tool failures, sandbox restrictions, mill daemon errors, or SSL/network issues. Provides targeted guidance and automated fixes for known blockers."
allowed-tools: Bash(cat *), Bash(ls *), Bash(find *), Bash(python3 *), Read, Edit, Write
metadata:
  version: 0.1.0
---

# Squire — morphir-scala Dev Environment Assistant

Squire diagnoses and unblocks common development environment issues in the morphir-scala project. Run `/squire doctor` to get a situational report and actionable fixes.

## Skills

### `squire doctor` — Environment Diagnostic & Guidance

Diagnoses the current dev environment and provides targeted unblocking instructions for known issues.

**When to invoke:** Any time a Bash tool call fails with a build, network, or sandbox error — especially before retrying. Squire checks for all known blockers in one pass.

---

## Known Issues & Resolutions

### 1. Mill daemon TCP socket blocked (Sandbox)

**Symptom:**
```
java.net.SocketException: Operation not permitted
  at mill.client.ServerLauncher$.connectToServer
Mill launcher failed.
```

**Cause:** The Claude Code sandbox blocks JVM TCP socket connections (`java.net.Socket.connect`). The mill client uses TCP to talk to the daemon — even when `localhost`/`127.0.0.1` are in the allowed hosts list, raw JVM NIO sockets are blocked at the syscall level.

**Fix:** Always use `--no-server` with mill in Bash tools:
```bash
./mill --no-server <target>
```

Or use the project wrapper which detects this automatically:
```bash
./morphir-local <command>
```

**Do NOT:** Try to fix this by adding `allowLocalBinding: true` to sandbox settings — it allows *binding* but not *connecting*. The correct setting would require `allowAllUnixSockets` or a managed policy change, neither of which is user-configurable.

**CLAUDE.md reminder:** This project's `CLAUDE.md` instructs to always use `--no-server` for mill Bash invocations.

---

### 2. Mill assembly `mainClass` detection warning

**Symptom:**
```
Could not detect the parent class of task morphir.main.mainClass.super.main.
```

**Cause:** Mill's assembly task tries to introspect the JVM parent class of `mainClass` at build time. When `mainClass` is a plain `def` returning a non-`App`/non-`Main` entry point (like `CommandsEntryPoint` from case-app), the introspection fails.

**Fix:** Already fixed in `morphir/package.mill` — `mainClass` is wrapped as a `Task`:
```scala
override def mainClass: T[Option[String]] = Task { Some("org.finos.morphir.cli.MorphirCliMain") }
```
If this warning reappears, verify the `Task { }` wrapper is present.

---

### 3. `cellar` temp file permission error

**Symptom:**
```
/var/folders/hc/.../cellar-*.tasty: Operation not permitted
```

**Cause:** Cellar (a native binary for JVM API inspection) writes temp `.tasty` files to macOS's real temp dir (`/var/folders/...`), which is outside the sandbox write allowlist.

**Fix:** Add `/var/folders` to the sandbox filesystem allowlist in `~/.claude/settings.json`:
```json
{ "sandbox": { "filesystem": { "allowWrite": ["/var/folders"] } } }
```
If this reappears, check that the user settings file still contains this entry.

> **Important:** Changes to `~/.claude/settings.json` sandbox settings only take effect after restarting Claude Code. If the entry is present but cellar still fails, restart the session.

---

### 4. `mise run setup` failing on elm-tooling downloads (CI)

**Symptom:**
```
curl: (22) The requested URL returned error: 504
error: postinstall script from "@morphir-examples/finance" exited with 1
```

**Cause:** `bun install` triggers `elm-tooling install` postinstall scripts which try to download elm binaries from GitHub releases. This flakes with HTTP 504 in CI.

**Fix:** Two mitigations already in place:
1. `mise run setup` skips postinstall scripts unless `ELM_TOOLING_INSTALL=1` is set
2. CI caches `~/.elm/elm-tooling` via `actions/cache@v4` keyed on `elm-tooling.json` files

To run setup with elm tooling locally:
```bash
ELM_TOOLING_INSTALL=1 mise run setup
```

---

### 5. Scalafmt lint failures

**Symptom:** CI `lint` job fails with `Found N misformatted files`

**Fix:** Run scalafmt before pushing:
```bash
./mill --no-server mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```
Note the `--no-server` flag (see issue #1 above).

---

## Diagnostic Workflow

When invoked as `/squire doctor`, perform these checks in order and report findings:

1. **Check sandbox TCP restriction** — read the mill daemon port from `out/mill-daemon/socketPort` if it exists, then attempt `python3 -c "import socket,errno; s=socket.socket(); s.settimeout(1); s.connect(('127.0.0.1', <port>))"`. If the error is `errno.EPERM` (Operation not permitted) the JVM socket sandbox is active. If `ConnectionRefused` the daemon isn't running but sockets work. If it succeeds, the daemon is reachable.

2. **Check mill daemon state** — read `out/mill-daemon/processId` and `out/mill-daemon/daemonState.json` if they exist

3. **Check `/var/folders` write access** — attempt `python3 -c "open('/var/folders/.squire-probe','w').close(); import os; os.unlink('/var/folders/.squire-probe')"` to verify cellar can write temp files

4. **Check `elm-tooling` skip** — verify `.config/mise/tasks/setup` contains the `ELM_TOOLING_INSTALL` guard

5. **Check `mainClass` Task wrapper** — grep `morphir/package.mill` for `Task { Some("org.finos.morphir.cli.MorphirCliMain") }`

6. **Check `~/.claude/settings.json`** — verify `/var/folders` is in `sandbox.filesystem.allowWrite`

Report each check as ✅ (no action needed) or ⚠️ (blocker present) with the specific fix to apply.
