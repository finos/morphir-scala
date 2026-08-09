# Squire PR #955 Policy Parity Design

## Goal

Preserve every Squire runtime and policy regression introduced by develop PR #955 in the unified Scala/Kyo implementation without restoring Python tooling or duplicating its test structure mechanically.

## Scope

The production ports already present in `SquireEnv`, `SquireCellar`, and `SquireDoctor` remain the source of truth. This change closes the remaining verification gaps in the Scala suite:

- GitHub workflow permissions, exact Mill Morphir selectors, JVM-platform membership, dependency ordering, and cache boundaries.
- Mise build delegates, dedicated local-CI steps, Mill-owned Morphir Elm provisioning, and the absence of a second Morphir Elm installation.
- Acquisition-cache behavior when disabled, unreadable, or bounded by entry-count and byte limits.
- Traceable coverage for every Python policy test added by PR #955.

Schema, repository, tracking, branch, and spec workflow behavior is outside this change.

## Design

### Hosted CI policy

Extend the existing `SquireCiPolicy` pure validators rather than introducing another parser or dependency. Each validator reads the real workflow text and enforces one contract with descriptive assertion failures. Parameterized mutation tables will exercise every protected field and prove that the validator rejects broadened permissions, selectors, dependencies, and cache paths.

The exact contracts are:

- top-level `contents: read` with no additional write capability;
- separate ordered Mill Morphir unit, integration, generated-project, fixture, and runtime jobs;
- the unit selector excludes published-plugin integration;
- generic JVM CI delegates to `test:jvm-platform`;
- the named JVM-platform task contains every non-classic JVM selector and no classic runtime selector;
- Morphir jobs cache only the verified acquisition cache and useful Mill outputs.

### Mise and provisioning policy

Extend `SquireMisePolicySpec` with file-backed checks against the maintained task definitions and package manifests. Build compatibility commands must delegate only to the named Mill IR tasks. Local CI must keep plugin integration and classic runtime work in dedicated steps. Setup must use `bun install --ignore-scripts`, and neither the root nor example project manifests may install a second `morphir-elm` tool.

These checks remain tests of repository behavior; no production policy framework will be added.

### Doctor edge behavior

Add parameterized `SquireDoctorSpec` cases for disabled cache mode, relative overrides while disabled, unreadable or changing entries, directory entry limits, and total hashing limits. The tests use real temporary filesystem entries where practical and injected platform environment only at the existing boundary.

Production doctor code changes are allowed only when a regression first fails because the required behavior is absent. Existing bounded, no-follow inspection semantics must be retained.

## Test Strategy

Work proceeds in RED/GREEN groups:

1. Add hosted-CI policy regressions and verify they fail against the under-constrained Scala validators.
2. Implement the smallest validator additions and rerun the focused suite.
3. Add Mise/provisioning regressions, observe RED, and add only test helpers or policy assertions needed for GREEN.
4. Add doctor edge regressions, observe RED, and modify production code only for demonstrated behavioral gaps.
5. Run the unified Squire suite, formatting/lint, and full local CI.

The final parity matrix will map every PR #955 Python test name to its Scala test group. The migration suite must continue to prove that Squire contains no Python or TypeScript implementation or tests.

## Constraints

- Keep the standalone Squire Mill pin exactly `1.2.0-RC1-24-042146`.
- Use Scala 3, Kyo Test, Kyo/Case App, and existing repository helpers only.
- Add no YAML, JSON, or workflow parsing dependency.
- Do not restore Python or Bun Squire runtime dependencies.
- Do not change GitHub publication permissions or perform any commit, push, PR, Beads, or Dolt publication without the existing approval boundaries.
- Preserve FINOS human-only commit authorship and omit tool attribution.

## Completion Criteria

- Every PR #955 Squire Python policy test has a documented Scala counterpart.
- Each new regression was observed failing before its implementation or validator change.
- `mise run test:squire`, `mise run lint`, and `mise run ci:local` exit successfully.
- The Squire Mill pin is unchanged and the worktree diff is cleanly formatted.
