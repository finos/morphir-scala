---
type: Intent
title: morphir-desktop Electron app
description: "Build morphir/desktop, an unpublished Electron app hosting morphir-ui with Scala.js in both processes over a kyo-jsonrpc seam."
state: Backlog
kind: feature
breaking: false
created: 2026-08-15
state_since: 2026-08-15
tags: [desktop, electron, ui]
---

# 0030 — morphir-desktop Electron app

Build morphir/desktop, an unpublished Electron app hosting morphir-ui with Scala.js in both processes over a kyo-jsonrpc seam.

## Problem

When a user works with Morphir on their own machine, they want a desktop application that opens a workspace and
lets them explore its IR and knowledge base, so they can work with local models without standing up a server or
a browser toolchain. No such application exists, and the stack it needs — a Scala UI running inside an Electron
host — has never been proven in this project.

## Approach

Add `morphir/desktop`, an unpublished Electron application. Its first release is deliberately thin: the IR
explorer and knowledge/intent browser from [`morphir-ui`](/0029-morphir-ui-kyo-ui-client-library.md), in a
shell that proves the architecture:

- Scala.js in both processes — the renderer mounts `morphir-ui`; the main process runs the v1 services through
  [`morphir-appkit-electron`](/0025-electron-appkit.md).
- One seam between them: kyo-jsonrpc routes over an Electron-IPC wire, with a tiny hand-written CommonJS
  preload. The renderer stays sandboxed and speaks only the service contract; no port is ever opened.
- The seam is the growth path: a later JVM sidecar serves the same routes over kyo-jsonrpc's stdio transport
  when workbench-scale features need the JVM stack. That work is deferred and out of scope here.
- Dev loop is `fastLinkJS` into Vite with `vite-plugin-electron`; packaging is electron-builder, arch-neutral
  while the app is pure JS.

The architecture evidence — Scala.js-vs-sidecar-vs-Wasm research — lives with the working notes; the decision
it produced is recorded across this intent, 0025, and 0029.

Blocked by 0029 (components and contract) and 0025 (the appkit leaf).
