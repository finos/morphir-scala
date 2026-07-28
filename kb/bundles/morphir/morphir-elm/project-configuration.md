---
type: Reference
title: Project Configuration
description: How morphir-elm consumes morphir.json, and the reference forms its dependency resolution accepts.
tags: [morphir-elm, configuration, morphir-json, dependencies]
status: stable
sources:
  - id: repo-morphir-json
    resource: https://github.com/finos/morphir-elm/blob/1956c36d3715851a2f215775a45395690746d801/morphir.json
    title: morphir-elm morphir.json
  - id: morphir-json-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/morphir-json/morphir-json-specification.md
    title: Morphir JSON Project Configuration Specification
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Project Configuration

`morphir.json` at the project root configures the toolchain. The format is specified in the `morphir-ir-v3` bundle's
project configuration concept; this concept covers what *this implementation* does with it.

## Required fields

`name`, `sourceDirectory`, and `exposedModules`. This repository's own file is a minimal example:

```json
{
    "name": "Morphir",
    "sourceDirectory": "src",
    "exposedModules": [
        "IR.Name", "IR.Path", "IR.QName", "IR.FQName", "IR.AccessControlled",
        "IR.Type", "IR.Value", "IR.Module", "IR.Package", "IR.Distribution",
        "IR.FormatVersion", "IR.Source"
    ]
}
```

`exposedModules` entries exclude the package prefix, so `"IR.Name"` under `name: "Morphir"` means the Elm module
`Morphir.IR.Name`. This list is what becomes the package's public specification — see [IR Module Map](/ir-api.md).

## Dependency resolution

`dependencies` and `localDependencies` hold references to other Morphir IR files. CLI2 owns their resolution and
accepts:

- **Data URL** — RFC 2397 `data:` containing JSON
- **File URL** — `file:`
- **Network URL** — `http:`, `https:`, `ftp:`
- **Local file path** — resolved first against the current working directory, then against the `morphir.json`
  directory

`git:`, `github:`, and `npm:` are **reserved but not implemented**. Code expecting them to work will fail.

`localDependencies` exists for backwards compatibility; in practice CLI2 resolves plain paths there by the same rules
as `dependencies`.

## Decorations

The `decorations` field maps a decoration id to a config with `displayName`, `ir` (path to the decoration schema IR),
and `entryPoint` (a `Package:Module:Type` reference). See [Decorations](/decorations.md).

## Related configuration

`elm.json` sits alongside it and configures the Elm compiler itself — this repository is an Elm `package` targeting
Elm `0.19`. `morphir mcp` creates both files when they are missing; see
[Command-Line Interface](/cli.md).
