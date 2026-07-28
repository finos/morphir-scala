---
type: Format
title: morphir.json Project Configuration
description: The project configuration file consumed by morphir-elm tooling, and the dependency reference forms it accepts.
tags: [morphir, configuration, morphir-json, tooling]
status: draft
sources:
  - id: morphir-json-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/morphir-json/morphir-json-specification.md
    title: Morphir JSON Project Configuration Specification
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# morphir.json Project Configuration

`morphir.json` is the project configuration file used by `finos/morphir-elm` and supported for compatibility by
Morphir Go tooling. It sits at the **project root** and is read by tools such as `morphir-elm make`.

> **Status.** The upstream specification marks itself as *Draft*, with `finos/morphir-elm` documentation as the
> authoritative source. `morphir.toml` is a separate, newer format specified elsewhere; the Morphir IR JSON format is
> unrelated — see [JSON Encoding and Format Versions](/json-encoding.md).

Keys use camelCase.

## Required fields

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `name` | string | Package name / module prefix; should be a valid Elm module name (e.g. `My.Package`) |
| `sourceDirectory` | string | Directory holding the Elm/Morphir sources, relative to the project root |
| `exposedModules` | array of string | Modules in the package's public interface, **excluding** the common package prefix |

If `name` is `"My.Package"`, then `exposedModules: ["Foo"]` refers to the Elm module `My.Package.Foo`.

## Optional fields

### `dependencies`

An array of references to other Morphir IR files to load as dependencies. `morphir-elm` accepts:

- **Data URL** — RFC 2397 `data:` form containing JSON
- **File URL** — a `file:` URL
- **Network URL** — `http:`, `https:`, or `ftp:` returning JSON
- **Local file path** — relative or absolute. CLI2 resolves it first against the current working directory, then
  against the `morphir.json` directory.

`git:`, `github:`, and `npm:` are **reserved but unimplemented** in both `morphir-elm` and Morphir Go. Tools may
reject them.

### `localDependencies`

An array of dependency references treated as "local" for backwards compatibility. In practice CLI2 resolves plain
paths here by the same rules as `dependencies`.

### `decorations`

A map from decoration id to a decoration config, declaring sidecar decoration schemas and where their values live.
Config fields include `displayName`, `ir` (path to the decoration schema IR file), and `entryPoint` (a fully-qualified
type reference of the form `Package:Module:Type`).

## Example

```json
{
    "name": "Morphir",
    "sourceDirectory": "src",
    "exposedModules": ["IR.Name", "IR.Path", "IR.Type", "IR.Value", "IR.Distribution"]
}
```

That is `morphir-elm`'s own `morphir.json` — see the sibling `morphir-elm` bundle for how the tooling consumes it.
