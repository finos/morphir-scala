---
type: Reference
title: Command-Line Interface
description: The morphir-elm and morphir commands, their generation subcommands, and the MCP server.
tags: [morphir-elm, cli, tooling, mcp]
status: stable
sources:
  - id: readme
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/README.md
    title: morphir-elm README — Usage
  - id: package-json
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/package.json
    title: morphir-elm package.json — bin
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Command-Line Interface

The npm package installs four executables:

| Command | Entry point |
| ------- | ----------- |
| `morphir-elm` | `cli/morphir-elm.js` — the original CLI |
| `morphir` | `cli2/lib/morphir.js` — the newer TypeScript CLI |
| `morphir-mcp` | `cli2/lib/morphir-mcp.js` — the MCP server |
| `morphir-dapr` | `cli/morphir-dapr.js` |

## `morphir-elm`

Five subcommands:

| Subcommand | Does |
| ---------- | ---- |
| `make` | Compiles Elm sources into IR, producing `morphir-ir.json` |
| `gen` | Reads that JSON and generates code into a target folder |
| `develop` | Serves a web UI for browsing the IR, reading the JSON `make` produced |
| `test` | Runs tests defined against the model |
| `treeview` | Renders a tree view of the IR |

`make` is the [frontend](/elm-frontend.md); `gen` is the [backends](/backends.md). Both are configured through
[`morphir.json`](/project-configuration.md).

## `morphir` (CLI2)

The newer CLI is organized as separate TypeScript entry points rather than one command file:

`morphir-make`, `morphir-init`, `morphir-scala-gen`, `morphir-json-schema-gen`, `morphir-typescript-gen`,
`morphir-snowpark-gen`, `morphir-test-coverage`, `morphir-stats`, `morphir-generate-test-data`, `morphir-dockerize`.

Where `morphir-elm gen` took a target flag, CLI2 gives each generation target its own command. CLI2 also owns
dependency resolution — it is the layer that interprets the `dependencies` reference forms described in
[Project Configuration](/project-configuration.md).

## `morphir mcp`

Starts a Model Context Protocol server so AI assistants and other MCP clients can work with a Morphir project.

```bash
morphir mcp --root-dir .
```

| Option | Meaning |
| ------ | ------- |
| `--root-dir <directory>` | Root of the Morphir project (required) |
| `--elm-command <command>` | Elm command to use for compilation (default `elm`) |

It exposes two tools — **`addModule`** (add a module with Elm code) and **`setTestCases`** (set test cases for
functions) — and creates `morphir.json` and `elm.json` if they are absent, so a project can be started from nothing.
Communication is over stdin/stdout per the MCP specification.
