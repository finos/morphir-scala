---
type: Intent
title: Morphir CLI buildkit integration
description: Run Morphir build and generation commands through the standard pipeline with production interpreters.
state: Cancelled
kind: feature
breaking: false
created: 2026-07-28
state_since: 2026-09-23
reason: "The Scala CLI retired; human-facing command orchestration belongs to the Rust CLI in finos/morphir."
issue: 932
tags: [buildkit, cli, pipeline]
---

# 0017: Morphir CLI buildkit integration

Run Morphir build and generation commands through the standard pipeline with production interpreters.

## Disposition

Cancelled on 2026-09-23 by the [CLI consolidation decision](../morphir/morphir-scala/decisions/0018-consolidate-the-cli-in-finos-morphir.md).
The Scala CLI retired, so this repository no longer plans general CLI orchestration. Reusable buildkit work
continues under its own intents. The retained MEP process host belongs to
[intent 0037](/0037-morphir-scala-elm-frontend-extension.md), which does not replace this intent's general command scope.
The problem and approach below record the abandoned proposal.

## Problem

The Scala CLI routed Elm make operations through an external `morphir-elm` process. If CLI commands
continue to own orchestration, the BDD-proven standard pipeline and production behavior can diverge.

## Approach

Make build and generation commands submit `BuildRequest` to the
[standard pipeline](/0009-standard-morphir-build-pipeline.md). Supply production workspace, package, checkpoint,
frontend, backend, artifact, logging, and progress interpreters at the CLI boundary, then translate `BuildOutcome`
into rendering and exit status.

Adopt this only after the required production interpreters exist. The CLI must not duplicate graph assembly or
introduce CLI-specific pipeline semantics.

Depends on the [BDD vertical slice](/0011-buildkit-bdd-vertical-slice.md), workspace and package interpreters,
[backend artifact reconciliation](/0014-backend-generation-and-artifact-reconciliation.md), and the relevant
frontend adapters.
