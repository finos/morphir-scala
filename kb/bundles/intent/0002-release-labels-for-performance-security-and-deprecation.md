---
type: Intent
title: "Release labels for performance, security and deprecation"
description: Add the three release-drafter labels the intent kind vocabulary needs.
state: Released
kind: build
breaking: false
created: 2026-07-28
state_since: 2026-07-28
tags: [release]
---

# 0002 — Release labels for performance, security and deprecation

Add the three release-drafter labels the intent kind vocabulary needs.

## Problem

The intent Kind vocabulary distinguishes performance, security and deprecation work, because each answers a different
question for a reader of release notes. The repository's release-drafter configuration had no labels for them, so all
three would have disappeared into `type: maintenance`.

## Approach

Add `type: performance`, `type: security` and `type: deprecation` to `.github/release-drafter.yml`, with their own
release-note sections. `deprecation` resolves to a minor bump — announcing a retirement is a compatibility-relevant
event; the other two resolve to patch.

Kind stays the intent-side vocabulary and the label stays the PR-side one; they are kept in step deliberately rather
than one deriving the other.
