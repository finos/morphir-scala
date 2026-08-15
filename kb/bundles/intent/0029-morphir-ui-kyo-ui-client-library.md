---
type: Intent
title: morphir-ui kyo-ui client library
description: "Publish morphir-ui, a kyo-ui client library with the IR explorer, knowledge browser, and the desktop service contract, shared by browser and Electron."
state: InProgress
kind: feature
breaking: false
created: 2026-08-15
state_since: 2026-08-15
tags: [ui, kyo-ui, appkit, electron]
---

# 0029 — morphir-ui kyo-ui client library

Publish morphir-ui, a kyo-ui client library with the IR explorer, knowledge browser, and the desktop service contract, shared by browser and Electron.

## Problem

Morphir captures business logic as data, but the project has no client surface for looking at it. When a user
opens a Morphir IR or the knowledge base, they want to browse packages, modules, types, values and concepts
visually, so they can understand the logic without reading JSON or Markdown source. Two clients need that
surface — the web browser and the planned Electron desktop app ([0030](/0030-morphir-desktop-electron-app.md))
— and without a shared library each would duplicate the other's UI code.

## Approach

Publish `morphir/ui` as `morphir-ui`, a kyo-ui library sitting at the top level like `runtime` — it carries
Morphir types, so it belongs to no publishing family ([decision 0013](../morphir/morphir-scala/decisions/0013-published-library-families.md)).
Two things live here:

- **Components**: the IR explorer and the knowledge/intent browser, written against kyo-ui so one value mounts
  in the browser (`UI.runMount`) and in the Electron renderer unchanged.
- **The service contract**: kyo-schema-typed protocol and kyo-jsonrpc route definitions the components consume
  — `IrService` and `KnowledgeService` (read-only) plus `ShellService` (host affordances the browser stubs).
  The contract is transport-blind; hosts choose the wire.

Scala.js first: the module starts JS-only, written platform-neutral so the JVM platform is a later
`package.mill.yaml` addition, not a rewrite. A Wasm link variant compiles in CI to keep that axis open.
Public APIs follow the Kyo-module rules: `Maybe`/`Result`, machinery in `internal`.
