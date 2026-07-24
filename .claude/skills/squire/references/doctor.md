# Squire Doctor — Diagnostic Reference

Full catalogue of known environment blockers and the step-by-step diagnostic procedure for `/squire doctor`.

---

## Diagnostic Workflow

Run these checks in order and report each as ✅ (no action needed) or ⚠️ (blocker present) with the specific fix to apply.

### 1. Mill daemon TCP connectivity

```bash
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/check-mill-daemon.py
```

Output `REACHABLE` → daemon accessible, no action needed.
Output `SANDBOX` → JVM TCP blocked; see [Mill daemon blocked](#1-mill-daemon-tcp-socket-blocked-sandbox).
Output `REFUSED` or `NO_DAEMON` → daemon not running; plain `./mill` will start one, or use `./morphir-local`.

### 2. `/var/folders` write access (cellar)

```bash
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/check-var-folders.py
```

Output `OK` → cellar can write temp files.
Output `BLOCKED` → see [cellar temp file error](#3-cellar-temp-file-permission-error).

### 3. Project configuration checks

```bash
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/check-project-config.py
```

Checks elm-tooling guard, `mainClass` Task wrapper, and `/var/folders` in sandbox settings in one pass.
Output `ISSUE` lines identify which fixes are needed.

---

## Known Issues & Resolutions

### 1. Mill daemon TCP socket blocked (Sandbox)

**Symptom:**
```
java.net.SocketException: Operation not permitted
  at mill.client.ServerLauncher$.connectToServer
Mill launcher failed.
```

**Cause:** The Claude Code sandbox *may* block JVM TCP socket connections (`java.net.Socket.connect`) depending on the active sandbox configuration in `~/.claude/settings.json` or managed settings. Even when `localhost`/`127.0.0.1` are in the allowed hosts list, raw JVM NIO sockets can be blocked at the syscall level by the sandbox policy. This is not a universal restriction — it depends on how Claude Code is configured for your environment.

**Fix:** Use `--no-server` with mill, or use the project wrapper which detects this automatically:
```bash
./mill --no-server <target>
# or
./morphir-local <command>
```

**Do NOT** try to fix this by adding `allowLocalBinding: true` to sandbox settings — it allows *binding* but not *connecting*.

---

### 2. Mill assembly `mainClass` detection warning

**Symptom:**
```
Could not detect the parent class of task morphir.main.mainClass.super.main.
```

**Cause:** Mill's assembly task tries to introspect the JVM parent class of `mainClass` at build time. `CommandsEntryPoint` (case-app) is not a traditional `App`/`Main` so introspection fails.

**Fix:** Already resolved in `morphir/package.mill` — `mainClass` is wrapped as a `Task`:
```scala
override def mainClass: T[Option[String]] = Task { Some("org.finos.morphir.cli.MorphirCliMain") }
```
If the warning reappears, verify the `Task { }` wrapper is still present.

---

### 3. `cellar` temp file permission error

**Symptom:**
```
/var/folders/hc/.../cellar-*.tasty: Operation not permitted
```

**Cause:** Cellar writes temp `.tasty` files to macOS's real temp dir (`/var/folders/...`). Depending on your Claude Code sandbox configuration, this path may be outside the write allowlist.

**Fix:** Add `/var/folders` to `~/.claude/settings.json`:
```json
{ "sandbox": { "filesystem": { "allowWrite": ["/var/folders"] } } }
```

> **Important:** Sandbox config changes require restarting Claude Code to take effect.

---

### 4. `mise run setup` failing on elm-tooling downloads (CI)

**Symptom:**
```
curl: (22) The requested URL returned error: 504
error: postinstall script from "@morphir-examples/finance" exited with 1
```

**Cause:** `bun install` triggers `elm-tooling install` postinstall scripts which download elm binaries from GitHub releases — flaky under network restrictions.

**Fix:** Two mitigations are in place:
1. `mise run setup` skips postinstall scripts unless `ELM_TOOLING_INSTALL=1` is set
2. CI caches `~/.elm/elm-tooling` via `actions/cache@v4`

To run locally with elm tooling:
```bash
ELM_TOOLING_INSTALL=1 mise run setup
```

---

### 5. Scalafmt lint failures

**Symptom:** CI `lint` job fails with `Found N misformatted files`

**Fix:**
```bash
./mill --no-server mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```
