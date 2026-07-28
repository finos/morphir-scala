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

## What this turned up

The three new labels were the smaller half. Of the nine `type:` labels `release-drafter.yml` referenced, only
`type: dependencies` actually existed in the repository — `feature`, `bug`, `breaking`, `maintenance` and `docs` had
never been created. Release-note categorisation and semver resolution had therefore been largely inert, with every
pull request except dependency updates falling through uncategorised.

All eight missing labels were created. The repository's default labels (`bug`, `enhancement`, `documentation`)
overlap in meaning but are not what the configuration reads; reconciling or retiring them is a separate call for
maintainers.
