---
type: Intent
title: Publish intent as GitHub issues
description: Project curated intent outward so contributors see planned work without reading the kb.
state: Cancelled
kind: feature
breaking: false
created: 2026-07-28
state_since: 2026-07-28
tags: [intent]
reason: "GitHub Issues is the inbox, not an output — projecting intent outward would create two-way drift."
---

# 0004 — Publish intent as GitHub issues

Project curated intent outward so contributors see planned work without reading the kb.

## Problem

Curated intent lives in the knowledge base, where an outside contributor is unlikely to look. Planned work is
therefore less visible than it would be on an issue tracker.

## Approach

Considered and rejected. GitHub Issues is the inbox — a surface anyone may write to — and projecting intent outward
would make it both an input and an output. Two systems that both accept writes for the same facts drift apart, and
reconciling them becomes a standing chore that nobody owns.

If outward visibility becomes a real need, a read-only projection (a generated page, or release notes derived from
Released intent) keeps the single writer while still publishing. That would be new intent, not a revival of this one.
