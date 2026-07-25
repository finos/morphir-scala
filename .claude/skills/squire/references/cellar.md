# Squire use-cellar — JVM API Inspection

[Cellar](https://github.com/VirtusLab/cellar) is a CLI tool that queries the public API of any Maven JVM dependency — symbol signatures, package listings, name search, and dependency trees. Squire wraps it with project-specific repository configuration and coordinate aliases.

---

## Prerequisites

### Installation

Cellar is managed via mise. Check if it is installed:

```bash
which cellar && cellar --version
```

If missing, install via mise:

```bash
mise install github-VirtusLab/cellar
```

### `/var/folders` write access

Cellar writes temp `.tasty` files to macOS's `/var/folders/`. If you hit `Operation not permitted`, run `/squire doctor` — see the cellar section for the fix.

### Private Maven repositories (optional)

For dependencies not on Maven Central, create a local settings file from the template:

```bash
cp .config/squire/settings.local.yaml.template .config/squire/settings.local.yaml
```

Then add your Maven mirror URLs to `settings.local.yaml` — it is gitignored. The wrapper loads it automatically. Without it, only Maven Central is used.

---

## Using the wrapper

All cellar queries for this project should go through `cellar-query.py`, which automatically adds the internal Maven repositories and supports coordinate aliases for common project dependencies.

```bash
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py <command> <coordinate> [args]
```

### Commands

**`get`** — Get all members of a type:

```bash
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py get <coordinate> <fully.qualified.Symbol>
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py get <coordinate> <fully.qualified.Symbol> --hide-inherited
```

**`search`** — Substring search for symbol names:

```bash
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py search <coordinate> <query>
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py search <coordinate> <query> --limit 20
```

**`deps`** — Show the dependency tree:

```bash
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py deps <coordinate>
```

---

## Coordinate aliases

The wrapper accepts short aliases for common project dependencies:

| Alias | Resolves to |
| ----- | ----------- |
| `case-app:2.1.0` | `com.github.alexarchambault:case-app_3:2.1.0` |
| `kyo-case-app` | `io.getkyo:kyo-case-app_3:1.0.0-RC5` |
| `kyo-schema` | `io.getkyo:kyo-schema_3:1.0.0-RC5` |
| `kyo-zio` | `io.getkyo:kyo-zio_3:1.0.0-RC5` |
| `zio:2.1.26` | `dev.zio:zio_3:2.1.26` |
| `zio-cli` | `dev.zio:zio-cli_3:0.8.1` |
| `mill-scalalib` | `com.lihaoyi:mill-scalalib_3:0.12.0` |
| `scala3-library` | `org.scala-lang:scala3-library_3:3.8.4` |

Full Maven coordinates are also accepted directly:

```bash
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py get "dev.zio:zio_3:2.1.26" "zio.ZIO"
```

---

## Common queries for this project

### case-app — CLI parsing

```bash
# All members of CommandsEntryPoint (the entry point base class)
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py get case-app:2.1.0 caseapp.core.app.CommandsEntryPoint

# Help[T] — what methods are available for customising help output
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py get case-app:2.1.0 caseapp.core.help.Help --hide-inherited

# Search for annotation types (HelpMessage, Name, AppName, etc.)
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py search case-app:2.1.0 "AppName"
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py search case-app:2.1.0 "HelpMessage"
```

### kyo — Effect system and CLI

```bash
# KyoCommand — the base class for CLI commands
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py get kyo-case-app "kyo.KyoCommand"

# What run overloads are available
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py search kyo-case-app "run"

# Dependency tree — what kyo-case-app pulls in
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py deps kyo-case-app
```

### ZIO — Effect system

```bash
# ZIO type members
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py get "zio:2.1.26" "zio.ZIO"

# Search for a specific method
python3 ${CLAUDE_PLUGIN_ROOT}/scripts/cellar-query.py search "zio:2.1.26" "provide"
```

---

## Why cellar instead of reading sources?

Cellar queries compiled TASTy/bytecode — it shows the exact public API as shipped in the published artifact, including synthetic methods, inlined definitions, and extension methods that may not be obvious from source. It is faster than downloading sources jars and grepping, and works even when sources aren't available.
