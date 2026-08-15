---
type: Intent
title: OKF knowledge library
description: "Publish morphir-knowledge-okf as an OKF model library that parses concept bodies through the markdown langkit."
state: Refinement
kind: feature
breaking: false
created: 2026-08-14
state_since: 2026-08-14
tags: [knowledge, okf]
---

# 0022 — OKF knowledge library

Publish morphir-knowledge-okf as an OKF model library that parses concept bodies through the markdown langkit.

## Problem

The knowledge base under `kb/` is a directory of OKF bundles that humans and agents read as markdown. There is no
published Scala library for that model. The kb skill carries its own parser and types, JVM-only, inside
`.claude/skills/kb`. Library users cannot depend on that, and GitHub ingest cannot share it.

`contrib/knowledge` is microkanren, not OKF, and is not the home for this library.

## Approach

Publish `morphir/knowledge/okf` as `org.finos.morphir::morphir-knowledge-okf`, compiling for JVM, JS, and Native.
Shared sources hold bundle, concept, and frontmatter types. Concept bodies parse through
[0021 Markdown langkit](/0021-markdown-langkit.md).

The mill path is `knowledge/okf`, not `kb` or a top-level `okf`, so a second encoding can sit beside OKF and so `kb/`
keeps meaning the document tree. See
[decision 0013](../morphir/morphir-scala/decisions/0013-published-library-families.md).

The kb skill does not move onto this library in this intent. Switching the skill is later work. `contrib/knowledge`
stays until [0027](/0027-stop-using-contrib-for-first-class-work.md) and a migration intent.

GitHub ingest is [0023](/0023-import-github-sources-into-okf.md), which depends on this library and on
[0020](/0020-github-graphql-connector.md). Ingest code may live in this module's shared sources once both
dependencies exist.

The narrative home is the
[published library families Design Note](../morphir/morphir-scala/design/published-library-families.md).
