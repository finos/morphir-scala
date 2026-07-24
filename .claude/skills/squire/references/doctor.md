# Squire Doctor — Diagnostic Reference

Full catalogue of known environment blockers and the step-by-step diagnostic procedure for `/squire doctor`.

---

## Diagnostic Workflow

Run these checks in order and report each as ✅ (no action needed) or ⚠️ (blocker present) with the specific fix to apply.

1. **Check sandbox TCP restriction** — read the mill daemon port from `out/mill-daemon/socketPort` if it exists, then run:
   ```bash
   python3 -c "
   import socket, errno
   port = int(open('out/mill-daemon/socketPort').read().strip())
   s = socket.socket()
   s.settimeout(1)
   try:
       s.connect(('127.0.0.1', port))
       print('REACHABLE - daemon accessible')
   except OSError as e:
       if e.errno == errno.EPERM:
           print('SANDBOX - JVM TCP blocked, use --no-server')
       else:
           print(f'REFUSED - daemon not running ({e})')
   "
   ```

2. **Check mill daemon state** — read `out/mill-daemon/processId` and `out/mill-daemon/daemonState.json` if they exist.

3. **Check `/var/folders` write access** — run:
   ```bash
   python3 -c "
   import os
   try:
       open('/var/folders/.squire-probe','w').close()
       os.unlink('/var/folders/.squire-probe')
       print('OK - cellar can write temp files')
   except PermissionError:
       print('BLOCKED - add /var/folders to sandbox.filesystem.allowWrite in ~/.claude/settings.json, then restart Claude Code')
   "
   ```

4. **Check `elm-tooling` skip guard** — verify `.config/mise/tasks/setup` contains `ELM_TOOLING_INSTALL`:
   ```bash
   grep -c "ELM_TOOLING_INSTALL" .config/mise/tasks/setup && echo "OK" || echo "MISSING"
   ```

5. **Check `mainClass` Task wrapper** — verify `morphir/package.mill` uses `Task { }`:
   ```bash
   grep "Task { Some" morphir/package.mill && echo "OK" || echo "MISSING - assembly will warn"
   ```

6. **Check `~/.claude/settings.json`** — verify `/var/folders` allowWrite entry:
   ```bash
   python3 -c "
   import json, pathlib
   s = json.loads(pathlib.Path.home().joinpath('.claude/settings.json').read_text())
   paths = s.get('sandbox',{}).get('filesystem',{}).get('allowWrite',[])
   print('OK' if '/var/folders' in paths else 'MISSING - add /var/folders to sandbox.filesystem.allowWrite')
   "
   ```

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

**Cause:** Cellar writes temp `.tasty` files to macOS's real temp dir (`/var/folders/...`), which is outside the default sandbox write allowlist.

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
