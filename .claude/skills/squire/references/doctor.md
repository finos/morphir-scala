# Squire Doctor — Diagnostic Reference

Full catalogue of known environment blockers and the step-by-step diagnostic procedure for `/squire doctor`.

---

## Diagnostic Workflow

Run these checks in order and report each as ✅ (no action needed) or ⚠️ (blocker present) with the specific fix to apply.

### 1. Mill daemon TCP connectivity

```bash
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/check-mill-daemon.py
```

- `PORT_OPEN` → daemon port responds to Python. **This does not guarantee JVM socket success** — the sandbox may still block `java.net.Socket`. If `./mill` subsequently fails with `Operation not permitted`, follow [Mill daemon blocked](#1-mill-daemon-tcp-socket-blocked-sandbox).
- `SANDBOX` → both Python and JVM sockets blocked; use `--no-server`.
- `REFUSED` or `NO_DAEMON` → daemon not running; plain `./mill` will start one, or use `./morphir-local`.

### 2. Effective JVM temp write access (cellar)

```bash
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/check-var-folders.py
```

- `OK` → cellar can write temp files under the reported `java.io.tmpdir`.
- `BLOCKED` → see [cellar temp file error](#3-cellar-temp-file-permission-error).
- `UNAVAILABLE` → Java is missing or its bounded property query failed; no path was assumed.

### 3. Project configuration checks

```bash
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/check-project-config.py
```

Checks:

- Mill-owned Morphir Elm setup
- the YAML-owned `mainClass` setting
- Mill Morphir plugin and local-repository wiring
- machine acquisition cache state
- metabuild freshness
- effective JVM temp writability

`ISSUE` lines identify which fixes are needed.

---

## Known Issues & Resolutions

### 1. Mill daemon TCP socket blocked (Sandbox)

**Symptom:**

```text
java.net.SocketException: Operation not permitted
  at mill.client.ServerLauncher$.connectToServer
Mill launcher failed.
```

**Cause:** The Claude Code sandbox *may* block JVM TCP socket connections (`java.net.Socket.connect`) depending on the active sandbox configuration in `~/.claude/settings.json` or managed settings. Even when `localhost`/`127.0.0.1` are in the allowed hosts list, raw JVM NIO sockets can be blocked at the OS level by the sandbox policy. This is not a universal restriction — it depends on how Claude Code is configured for your environment.

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

```text
Could not detect the parent class of task morphir.main.mainClass.super.main.
```

**Cause:** Mill's assembly task tries to introspect the JVM parent class of `mainClass` at build time. `CommandsEntryPoint` (case-app) is not a traditional `App`/`Main` so introspection fails.

**Fix:** Already resolved in `morphir/package.mill.yaml`:

```yaml
mainClass: org.finos.morphir.cli.MorphirCliMain
```

If the warning reappears, verify the YAML entry is still present.

---

### 3. `cellar` temp file permission error

**Symptom:**

```text
/var/folders/hc/.../cellar-*.tasty: Operation not permitted
```

**Cause:** Cellar writes temp `.tasty` files to the JVM's active `java.io.tmpdir`. On macOS this is normally a user-specific directory below `/var/folders`, not the `/var/folders` root.

**Fix:** Probe a writable path, then pass it directly to the native Cellar process:

```bash
JAVA_TOOL_OPTIONS="-Djava.io.tmpdir=<writable-temp>" python3 .claude/skills/squire/scripts/check-var-folders.py
python3 .claude/skills/squire/scripts/cellar-query.py --temp-directory "<writable-temp>" CELLAR_COMMAND CELLAR_COORDINATE CELLAR_ARGUMENTS
```

`JAVA_TOOL_OPTIONS` configures the Java probe. Cellar is a native executable, so its wrapper passes the temp setting as a native runtime option.

---

### 4. `mise run setup` must not provision Morphir Elm

**Symptom:**

```text
curl: (22) The requested URL returned error: 504
error: postinstall script from "@morphir-examples/finance" exited with 1
```

**Cause:** Workspace postinstall hooks download Elm binaries, and legacy workspace dependencies installed a second
Morphir Elm CLI outside Mill's verified toolchain.

**Fix:** `mise run setup` always uses `bun install --ignore-scripts`; Morphir Elm dependencies and make scripts are
absent from the root workspace manifests. Mill provisions the locked compiler and owns fixture generation. Optional
Elm editor/formatting tooling remains a developer-local concern and is not part of the build contract.

---

### 5. Scalafmt lint failures

**Symptom:** CI `lint` job fails with `Found N misformatted files`

**Fix:**

```bash
./mill --no-server mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```

### 6. Mill Morphir plugin workflow

Read [mill-morphir.md](mill-morphir.md) for the fast and dogfood routes.

The project check may report:

- **Missing plugin module:** verify the plugin tree with `./mill resolve 'mill-plugins.morphir.__'`.
- **Broken local repository wiring:** run `./mill mill-plugins.morphir.integration.test`.
- **Corrupt cache entry:** rerun `./mill examples.morphir-elm-projects.evaluator-tests.morphirIR` online. Verified acquisition replaces bad bytes.
- **Disabled machine cache:** generation remains correct but downloads stay task-local. Set `MORPHIR_NODE_DISABLE_MACHINE_CACHE=false` when reuse is wanted.
- **Stale metabuild:** run `./mill resolve 'mill-plugins.morphir.__'` to recompile it.

Squire only diagnoses these states. Mill owns acquisition, generation, and fresh-consumer acceptance.
