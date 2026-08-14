---
type: Intent
title: Import GitHub sources into OKF
description: "Map GitHub issues, pull requests, and discussions onto OKF concept documents through the GitHub connector."
state: Backlog
kind: feature
breaking: false
created: 2026-08-14
state_since: 2026-08-14
tags: [knowledge, github, okf]
---

# 0023 — Import GitHub sources into OKF

Map GitHub issues, pull requests, and discussions onto OKF concept documents through the GitHub connector.

## Problem

Issues, pull requests, and discussions on GitHub are a source of project knowledge that today never enters the OKF
tree unless someone copies them by hand. The knowledge base therefore misses conversation that already happened in
the inbox. Exporting OKF back onto GitHub is a possible later need; import is the first direction.

This mapping mentions OKF types, so it is not a connector. Intent 0004 (project intent outward as GitHub issues)
stays Cancelled: GitHub remains a source, not a second writer for curated intent.

## Approach

Once [0020](/0020-github-graphql-connector.md) and [0022](/0022-okf-knowledge-library.md) exist, add ingest in
`morphir/knowledge/okf` that turns a GitHub issue, pull request, or discussion into one or more OKF concept
documents. The mapping lives in shared sources so it follows the connector onto JS and Native. JVM-only pieces, if
any, go in `jvm/src`.

The connector stays GitHub-shaped. OKF types stay in `knowledge.okf`. This intent owns the boundary between them.

Export of OKF onto GitHub, and switching the kb skill onto the ingest API, are out of scope.

Depends on 0020 and 0022. The narrative home is the
[published library families Design Note](../morphir/morphir-scala/design/published-library-families.md).
