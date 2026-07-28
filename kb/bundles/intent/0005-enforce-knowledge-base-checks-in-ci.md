---
type: Intent
title: Enforce knowledge base checks in CI
description: "Run kb check and intent check on every pull request, so the obligations bite instead of being advisory."
state: Released
kind: build
breaking: false
created: 2026-07-28
state_since: 2026-07-28
tags: [ci, kb]
capability: morphir/morphir-scala:/continuous-integration.md
---

# 0005 — Enforce knowledge base checks in CI

Run kb check and intent check on every pull request, so the obligations bite instead of being advisory.

## Problem

`kb check` and `intent check` exit non-zero on errors, but nothing ran them. Every argument for the knowledge base not
falling behind the code — the required capability link, the state obligations — rested on enforcement that was
entirely voluntary.

## Approach

A `knowledge-base` job in the CI workflow, and a matching `mise run kb:check` task so local and CI behaviour are the
same command.

The job needs a JVM and nothing else, because the kb skill is a self-contained Mill script. Provenance checks are
skipped on the runner: they compare against reference checkouts under `.refs/`, which is gitignored and absent there.

Errors fail the build; warnings do not. Staleness is a warning by design, and a warning that fails CI is a warning
people route around.

The aggregate `ci` job depends on it, so the gate is real.
