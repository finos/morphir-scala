---
type: Intent
title: Electron appkit
description: "Publish morphir-appkit-electron for running Morphir inside an Electron host using Scala."
state: Superseded
kind: feature
breaking: false
created: 2026-08-14
state_since: 2026-08-28
tags: [appkit, electron]
superseded_by: 0039
---

# 0025 — Electron appkit

Publish morphir-appkit-electron for running Morphir inside an Electron host using Scala.

## Problem

Morphir tooling that should run as a desktop application needs a place to live that is neither a library bridge
(`kit`) nor an external-system client (`connector`). Electron is a host application. Integrating Morphir into it
is host-app work, and today there is no mill namespace that says so.

## Approach

Add `morphir/appkit/electron` publishing as `morphir-appkit-electron`, a Scala.js (JS-platform) module.

The embedding mechanism, settled in Refinement: Scala inside Electron means Scala.js in both processes. This
leaf owns the host-side machinery that makes it work:

- minimal typed facades over the Electron APIs the main process needs (windows, menus, dialogs, IPC);
- a main-process bootstrap for hosting Kyo applications on Electron's Node runtime as an ESM bundle;
- a kyo-jsonrpc wire transport over Electron IPC (`ipcMain`/`ipcRenderer` via `contextBridge`), so hosted
  apps expose services to their sandboxed renderer without opening a port;
- a `SecretStore` bridge backed by Electron `safeStorage`.

A JVM-sidecar variant (same routes over kyo-jsonrpc stdio, LSP-style) is the anticipated growth path for
workbench workloads; it is deferred and not part of this intent.

First consumers: [`morphir-ui`](/0029-morphir-ui-kyo-ui-client-library.md) components hosted by the
[`morphir/desktop`](/0030-morphir-desktop-electron-app.md) app.

The narrative home is the
[published library families Design Note](../morphir/morphir-scala/design/published-library-families.md).
The family rule is [decision 0013](../morphir/morphir-scala/decisions/0013-published-library-families.md).
