# Dedicated Squire Policy CI Job

## Context

The GitHub Actions `lint` job currently runs both `mise run lint` and
`mise run test:squire`. GitHub therefore reports Squire policy failures under
the `lint` check even though the Mise lint task does not depend on Squire. This
placement was introduced as a convenient release-policy gate and is now
enforced by the Scala Squire CI policy suite.

## Decision

Create a top-level `squire-policy` job that runs in parallel with `lint` and
the other independent CI jobs. The job will:

1. Check out the current branch with full history.
2. Install the configured Java version.
3. Restore Coursier dependencies.
4. Install Mise.
5. Run exactly `mise run test:squire`.

The `lint` job will retain only `mise run lint`. The aggregate `ci` job will
add `squire-policy` to its `needs` list, so Squire policy remains a required
part of the aggregate gate without being mislabeled as lint.

`squire-policy` will have no `needs` declaration. It can therefore run in
parallel with lint, knowledge-base validation, platform tests, and the first
Mill Morphir capability job.

## Policy Validation

`SquireCiPolicySpec` will define the CI boundary rather than merely matching a
convenient step location. It will require:

- exactly one top-level `squire-policy` job;
- exactly one `Test Squire and release policy` step in that job;
- the step to run exactly `mise run test:squire`;
- no `mise run test:squire` invocation in `lint` or any other job;
- no `needs` declaration on `squire-policy`;
- `squire-policy` to appear exactly once in the aggregate `ci.needs` list.

Mutation tests will reject a missing or duplicate job, a policy step moved
back into lint or another job, a changed command, a dependency added to the
policy job, and removal or duplication of the aggregate dependency.

## Scope

This change modifies only `.github/workflows/ci.yml` and the Scala Squire CI
policy tests. It does not change `mise run lint`, `mise run test:squire`, the
standalone Squire Mill pin, release behavior, product test selectors, or any
Python/Bun tooling.

## Verification

Implementation will follow RED/GREEN TDD:

1. Change the policy regression to require the dedicated job and confirm it
   fails against the current workflow.
2. Move the workflow step into the new job and add the aggregate dependency.
3. Run `mise run fmt`, `mise run test:squire`, pin/residue/diff checks,
   `mise run lint`, and `mise run ci:local` sequentially.
4. Push the existing feature branch and require PR #956 checks to return green
   with separate `lint` and `squire-policy` checks.
