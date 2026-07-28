---
type: Intent
title: Test suite for the kb and intent skill
description: "Cover the skill's parsing, scaffolding, checks and index with an executable suite that runs in CI."
state: Released
kind: test
breaking: false
created: 2026-07-28
state_since: 2026-07-28
tags: [kb, ci]
capability: morphir/morphir-scala:/knowledge-base-tooling.md
---

# 0006 — Test suite for the kb and intent skill

Cover the skill's parsing, scaffolding, checks and index with an executable suite that runs in CI.

## Problem

Roughly 2,500 lines of Scala that mutate files on disk, verified only by running commands by hand and reading the
output. The Codex review on #936 found a frontmatter-corrupting bug that manual testing had walked straight past —
releasing an intent whose frontmatter ended in a `sources:` block inserted the new key between the block header and
its children.

Manual verification also cannot be re-run cheaply, so nothing stops a fixed bug coming back.

## Approach

`KbTests.scala` — an executable suite covering parsing, path arithmetic, scaffolding, the intent lifecycle and its
obligations, the checks, refresh, and the SQLite index.

Written with kyo-test, the framework `langkit` and `kit` already use. Mill's script mode exposes no test module, so
`mill test <script>` does not resolve — but kyo-test ships a CLI runner with a `main`, so the suite runs through it as
the script's `mainClass`, with `//| resources:` putting the ServiceLoader registry on the classpath. A hand-rolled
harness was written first and thrown away once that worked; using the project's own framework was plainly better.

Forty-nine cases across six suites. Four are named *regression* and pin behaviour that was once wrong — three from
the review, one from dogfooding. The suite was verified to fail: reintroducing the `setKeys` bug fails that case and
exits 1.

Discovery is ServiceLoader-only, so a suite missing from the registry silently never runs rather than failing —
which would leave CI green over untested code. `KbMetaSpec` asserts the registry and the defined suites agree, on the
same principle as the rest of this design: enforce it, do not merely document it.

Runs in CI in the `knowledge-base` job, and locally as `mise run kb:test`.
