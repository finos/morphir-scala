---
type: Decision Record
title: Keep compiling Mill Morphir plugins into the metabuild
description: "Normal builds continue to compile mill-plugins/morphir sources into the metabuild; pinned Central artifacts are deferred until bootstrap experience is measured."
state: Accepted
decided: 2026-08-14
tags: [mill, plugins, bootstrap, publishing]
status: stable
---

# 0012 — Keep compiling Mill Morphir plugins into the metabuild

Normal builds continue to compile the current `mill-plugins/morphir/` sources into the metabuild. They do **not**
switch to resolving a pinned `org.finos.morphir.mill` artifact from Maven Central for day-to-day development.

The five plugins still publish to Sonatype (SNAPSHOT from `develop`, releases from `main`) under
`MorphirMillPublishModule`. Publication and metabuild consumption are separate choices.

## Why

The [Mill Morphir plugin architecture](/design/mill-morphir-plugin-architecture.md) already gates a pinned-artifact
flip on measured maintenance and bootstrap experience after the first Mill 1.x plugin release. Enabling Sonatype
publication does not, by itself, supply that measurement:

- Clean checkouts already compile plugin sources into the metabuild with no bootstrap command
  (`mill-build/build.mill` appends those source roots). That path is known to work and needs no network.
- The authoritative dogfood boundary remains the local-SNAPSHOT fresh-consumer suite
  (`mill-plugins.morphir.integration.test`). Switching the metabuild to Central must not weaken that path.
- Central SNAPSHOT coordinates are mutable and, per Sonatype policy, may be cleaned up after about 90 days. Pinning
  normal builds to them would couple every contributor bootstrap to snapshot retention and network reachability.
- There is not yet measured evidence that a pinned release/milestone artifact reduces maintenance cost enough to
  justify the extra bootstrap failure modes (version skew between metabuild and source tree, forced upgrade cadence,
  offline clones).

So the first Mill 1.x publication lands the consumer-facing coordinates without changing how this repository builds
itself.

## Alternatives considered

1. **Flip immediately to a pinned Central SNAPSHOT in the metabuild.** Rejected: SNAPSHOT mutability and retention
   make it a poor pin for every clean checkout, and there is no bootstrap-experience evidence yet.
2. **Flip immediately to a pinned non-SNAPSHOT release/milestone.** Rejected for now: no release has been cut that
   includes the plugin family, and forcing a release solely to change the metabuild would invert the dependency
   between publication and measured need.
3. **Keep source metabuild (chosen).** Preserves zero-bootstrap clean checkouts and leaves publication free to
   serve external Mill consumers and the local-SNAPSHOT acceptance path.

## Consequences

- `mill-build` keeps compiling `mill-plugins/morphir/{toolchain,javascript,elm-tooling,core,elm}/src`.
- External consumers may depend on published `org.finos.morphir.mill:mill-morphir-*_mill1_3` coordinates once CI
  publishes them; this repository does not dogfood those coordinates in the metabuild yet.
- The local-SNAPSHOT integration suite remains the gate that proves the published boundary.

## Revisit when

Revisit (and supersede this record) only when **all** of the following are true:

1. At least one non-SNAPSHOT plugin release or milestone has been published and used by an external consumer, **or**
   a sustained period of `develop` SNAPSHOT publication has produced concrete bootstrap/maintenance data.
2. Measured evidence shows that a pinned artifact improves contributor or CI bootstrap enough to outweigh version
   skew and network/offline costs.
3. The flip can be implemented without weakening `mill-plugins.morphir.integration.test` (local-SNAPSHOT acceptance
   stays authoritative).
