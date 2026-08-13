---
type: Reference
title: Task-graph comparative survey
description: "Compare outcome models, identity minting, progress schemas, and deterministic ordering across Bazel, Buck2, Gradle, GitHub Actions, GitLab CI, BSP, ZIO, and Kyo for task-graph executor design."
tags: [pipeline, buildkit, comparative]
status: draft
sources:
  - id: bazel-bep
    resource: https://bazel.build/remote/bep
    title: Bazel Build Event Protocol
  - id: bazel-bep-proto
    resource: https://github.com/bazelbuild/bazel/blob/master/src/main/java/com/google/devtools/build/lib/buildeventstream/proto/build_event_stream.proto
    title: Bazel build_event_stream.proto
  - id: buck2-build-report
    resource: https://buck2.build/docs/users/build_observability/build_report/
    title: Buck2 build report
  - id: buck2-anon-targets
    resource: https://buck2.build/docs/rule_authors/anon_targets/
    title: Buck2 anonymous targets
  - id: gha-contexts
    resource: https://docs.github.com/en/actions/learn-github-actions/contexts
    title: GitHub Actions contexts
  - id: gradle-cli
    resource: https://docs.gradle.org/current/userguide/command_line_interface.html
    title: Gradle command line interface
  - id: bsp-spec
    resource: https://build-server-protocol.github.io/docs/specification
    title: Build Server Protocol specification
  - id: gradle-tooling-events
    resource: https://docs.gradle.org/current/javadoc/org/gradle/tooling/events/package-summary.html
    title: Gradle Tooling API progress events
  - id: bazel-4538
    resource: https://github.com/bazelbuild/bazel/issues/4538
    title: Bazel reproducible report-order feature request
---

# Task-graph comparative survey

Compare outcome models, identity minting, progress schemas, and deterministic ordering across Bazel, Buck2, Gradle, GitHub Actions, GitLab CI, BSP, ZIO, and Kyo for task-graph executor design.

The systems that got task-graph reporting right share four moves: they split "skipped by rule" from "blocked by
a failed dependency", they record failure causes as stable references rather than copied text, they mint child
identity from domain values rather than positions, and they get reproducible reports by re-sorting against a
total order over identities rather than by constraining the scheduler. This survey records the evidence behind
those claims. It serves the buildkit task-graph capability; its narrative home is the
[buildkit task-graph design note](../morphir/morphir-scala/design/buildkit-task-graph.md).

Claims here summarize public documentation and source read on 2026-08-13; the cited sources are the record.

## Outcome and status models

| System | Status vocabulary | Skip when a dependency fails | Cause shape | Defect vs domain failure |
| --- | --- | --- | --- | --- |
| GitHub Actions | status queued/in_progress/completed; conclusion success/failure/cancelled/skipped/neutral/timed_out; steps expose pre-policy `outcome` vs post-policy `conclusion` | conclusion `skipped`, same value as a condition skip; no blocker recorded | flat, single | none: `failure` covers script and infra alike |
| Gradle | SUCCESS, FAILED, UP-TO-DATE, FROM-CACHE, SKIPPED, NO-SOURCE | none: blocked tasks never appear, absence is the record | exception chains per failure; blocked tasks carry nothing | convention only, everything is a Throwable |
| Bazel (BEP) | `TargetComplete{success}` or `Aborted` with reason enum (SKIPPED, INCOMPLETE, INTERNAL, OUT_OF_MEMORY, ...) | distinct: `Aborted(SKIPPED)` for rule skips, `Aborted(INCOMPLETE)` for halted builds, failed `TargetComplete` under keep_going | root causes as references to failing action events, plural | partial, via the reason enum |
| Buck2 (report) | SUCCESS / FAIL per configured target plus an errors list | a blocked target is FAIL with errors pointing upstream; no distinct not-attempted status | flat list with `cause_index` dedup, documented unstable | none in the report |
| ZIO | `Exit[E, A]`; `Cause` tree: Fail, Die, Interrupt, composed by Then and Both | not applicable; Interrupt models stopped-before-result | full tree, lossless | strong three-way |
| Kyo | `Result[E, A]`: Success, Failure, Panic | not applicable | flat, single | strong two-way; no interrupt case |
| GitLab CI | success, failed, canceled, skipped, manual, and scheduling states | `skipped`, indistinguishable from rule skips | flat; `failure_reason` strings | none; `allow_failure` keeps raw `failed` and neutralizes it at pipeline level |

What the models teach:

- The shared flaw in GitHub Actions, GitLab, and Gradle is one skip bucket, or no record at all, for
  blocked-by-failure. Bazel proves rule skips and causal blocking are different statuses; consumers filtering
  "what went wrong" need Failed and Blocked without seeing rule skips.
- Bazel's root-cause attribution is structural: a failed target references the failing action events, so long
  blocked chains carry the leaf cause without copying prose. Buck2's string-dedup cache exists for the same
  reason, and its documented-unstable `cause_index` warns that cause identity should be a stable id.
- ZIO's `Cause` tree is lossless, but consumers overwhelmingly flatten it, and an executor's DAG already
  encodes the sequential and parallel composition between nodes. A flat Kyo-shaped cause per node suffices,
  with one addition: a suppressed-errors list for cleanup-after-failure, the one sequential composition the
  DAG does not capture.
- GitHub Actions' `outcome` versus `conclusion` and GitLab's `allow_failure` agree: tolerance policy never
  rewrites the node's raw outcome. The documented `needs.result` trap is what happens otherwise.
- Gradle's UP-TO-DATE and FROM-CACHE work because they are success from a consumer's view. Model cache reuse
  as a provenance dimension inside the success case, not as sibling statuses, and pattern matches stay total
  when incrementality arrives.

## Identity minting

| System | Identity form | Minted by | Uniqueness scope | Fan-out child identity |
| --- | --- | --- | --- | --- |
| Bazel | label `@repo//pkg:name` | directory position plus user-declared target name | per package, checked at load | actions keyed by owning target plus output paths; cache keys are content hashes, never display names |
| Gradle | task path `:proj:task` | project hierarchy plus user-declared task name | per project, duplicate registration fails eagerly | none at runtime; users mangle names for parameterized tasks |
| Mill | selector `module[key].task` | object nesting plus user-declared cross values | per module tree; values must be distinct and path-safe | by value, not position: reordering cross values does not shift identity |
| GitHub Actions | job id (YAML key), display name gets matrix values appended | YAML author; runner renders matrix values | job id per workflow file; display names not unique | matrix values in declaration order; name-string matching (required checks) breaks on rename or duplicates |
| Buck2 | label plus configuration hash; anon targets keyed by attribute hash | user label; system-minted hashes | per package; per (label, configuration) | attribute hash dedups identical work; hash paired with a display form |

What the identity survey teaches:

- Every mature system converged on scope plus user-declared leaf, validated eagerly. No surveyed system uses
  list position as user-facing child identity; Mill cross values, GitHub Actions matrix values, and Buck2
  attribute hashes all key by domain value, because positional identity renumbers siblings on insertion.
- Position-derived scope prefixes are the fragile half: subtree moves break absolute paths in every system.
  A sealed immutable pipeline is better placed than any of them, because the tree is frozen at seal; the
  hazard survives only across pipeline versions.
- Identity and display label must be separate fields. GitHub Actions matches branch-protection checks by
  rendered name, and renames silently orphan them.
- Hash identities (Buck2) are stable and collision-free but opaque; they work as identity of record only when
  paired with a human display form.

## Progress event schemas

| System | Event kinds | Linkage | Pairing rule | Diagnostics location |
| --- | --- | --- | --- | --- |
| Bazel BEP | ~20 payload kinds on one envelope | event ids plus children lists forming a DAG | no start/finish pairs; announced events must eventually close, aborts close them | per-node events only; finish carries an overall exit code |
| BSP | taskStart, taskProgress, taskFinish | `TaskId{id, parents}` plus originId | finish must follow a start with the same id | split across finish payloads and a parallel diagnostics channel; documented drift hazard |
| Gradle Tooling API | typed Start/Status/Finish per operation family | descriptor with parent | strict one-finish-per-start | in finish results; build-level failure repeats them, clients dedupe |
| Buck2 | SpanStart, SpanEnd, Instant, Record | trace/span/parent ids, replayable log | span pairing per id | in span-end payloads; console text is a separate instant event |

The convergent minimum is BSP's three verbs with Gradle's typed finish result: run-started and run-finished,
node-started and node-finished carrying the status, optional node-progress. Two rules make replay
deterministic: every finish pairs with exactly one earlier start, and every started node closes even on abort.
Consumers key on ids, never on event position. Diagnostics need one owner; BSP's parallel channels invite the
counts-versus-content drift its ecosystem documents, while Bazel and Buck2 keep per-node detail in one place.

## Deterministic ordering

- Bazel streams in completion order and documents no reproducibility; a reproducible-order request has been an
  open feature request since 2018.[^bazel-4538] Its end-of-run test summary is deterministic because it
  re-sorts by a comparator over status and identity.
- Gradle's `--continue` collects failures in completion order with no documented determinism.
- GitHub Actions matrix children order by declaration of the value arrays: declaration-derived, so stable.
- The extracted rule: reproducible reports come from a total order over identities fixed ahead of execution,
  never from constraining the scheduler.

[^bazel-4538]: Bazel issue 4538, predictable build sequence, open P3.
