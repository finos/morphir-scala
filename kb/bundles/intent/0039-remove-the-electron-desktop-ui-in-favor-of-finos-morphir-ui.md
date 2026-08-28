---
type: Intent
title: Remove the Electron desktop UI in favor of finos/morphir-ui
description: Retire the morphir-desktop Electron app and its appkit-electron secret-storage integration now that finos/morphir-ui ships the replacement desktop/web UI shell.
state: InProgress
kind: removal
breaking: true
created: 2026-08-28
state_since: 2026-08-28
tags: [desktop, electron, ui]
---

# 0039 — Remove the Electron desktop UI in favor of finos/morphir-ui

Retire the morphir-desktop Electron app and its appkit-electron secret-storage integration now that finos/morphir-ui ships the replacement desktop/web UI shell.

## Problem

When a contributor works on morphir-scala's Electron desktop app (`morphir/desktop`) or its
`morphir/appkit/electron` secret-storage integration, they want the code they touch to be the
project's one live UI effort, so their changes matter and their review time is not spent on a
surface a maintainer is planning to retire. UI development has centralized in
[finos/morphir-ui](https://github.com/finos/morphir-ui), which now ships the desktop and web UI
shell that `morphir/desktop` used to provide. Maintaining two Electron shells — one here, one
there — duplicates review burden and build/release machinery (the `morphir.desktop` Mill modules,
the `ci.desktop` release destination, the `desktop-matrix`/`-package`/`-verify`/`-release` CI jobs,
and the `morphir-appkit-electron` artifact) for a capability only one of them is meant to own going
forward.

## Approach

morphir-scala sheds the Electron desktop UI and the build/release tooling that shipped it, and
remains a library and capability provider: `morphir/ui` (the kyo-ui client library, still shared by
the local web host started by `morphir server`), `morphir/langkit/markdown` (including its kyo-ui
Markdown compiler), and `morphir/connector/github` (headless token plumbing) all stay. Only the
Electron-specific consumer (`morphir/desktop`) and the Electron-specific `appkit` backend
(`morphir/appkit/electron`) retire; the JVM/native `SecretStore` backends in `morphir/appkit` are
unaffected. Published `morphir-desktop-*` and `morphir-appkit-electron` artifacts stop with this
release — hence `breaking: true`.
