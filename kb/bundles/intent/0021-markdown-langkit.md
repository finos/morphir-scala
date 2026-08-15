---
type: Intent
title: Markdown langkit
description: "Publish a cross-platform markdown langkit that produces a CST on JVM, JS, and Native."
state: Refinement
kind: feature
breaking: false
created: 2026-08-14
state_since: 2026-08-14
tags: [langkit, markdown]
---

# 0021 — Markdown langkit

Publish a cross-platform markdown langkit that produces a CST on JVM, JS, and Native.

## Problem

OKF concept bodies are markdown, and the kb skill already parses them with `commonmark-java`, which is JVM-only.
Library users and `morphir-knowledge-okf` need the same parse on JVM, JS, and Native. Markdown is a source language,
so the work belongs in `langkit`, beside Elm, not in `kit` or `connector`.

## Approach

Publish `morphir/langkit/markdown` as `org.finos.morphir::morphir-langkit-markdown`. It depends on `langkit.core` for
`Span` and diagnostics. A `QueryableTree` instance is later work against `langkit.trees`.

`commonmark-java` must not enter the module. The parser that ships has to run on all three platforms, either as one
cross-platform library or as a shared AST with per-platform engines. Until that parser is named, the module ships a
stub that parses ATX headings and paragraphs into a CST so tests run everywhere with no third-party parser.

The first tests parse a handful of CommonMark fixtures to that CST on JVM, JS, and Native.

Depends on [decision 0013](../morphir/morphir-scala/decisions/0013-published-library-families.md).
[0022](/0022-okf-knowledge-library.md) depends on this intent for concept bodies.
The narrative home is the
[published library families Design Note](../morphir/morphir-scala/design/published-library-families.md).
