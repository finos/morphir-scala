# Squire Follow-up Review Fixes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the JVM temp probe ownership-safe, enforce Case App's repeated-`--sparse` contract, and preserve failed text-mode `kb check` diagnostics exactly.

**Architecture:** Create a unique temp probe atomically and clean only the owned entry; keep `reference repo add` on the strict generic required-argument boundary while passing Case App's decoded sparse list through unchanged; append non-empty text diagnostics with minimum separators and no trimming.

**Tech Stack:** Scala 3, Kyo RC6, kyo-case-app/Case App 2.1.0, kyo-test, Mill `1.2.0-RC1-24-042146`.

## Global constraints

- The public form is `squire reference repo add URL --sparse PATH --sparse PATH`; every path repeats the flag.
- Preserve positional and named URL forms and repeated sparse-path order.
- Reject extra positionals, duplicate named/positional URLs, arguments after `--`, and missing URLs before filesystem or process effects.
- Create probes with `CREATE_NEW`, `WRITE`, and `NOFOLLOW_LINKS`; delete only an entry successfully created by this check.
- Preserve non-empty failed text stdout/stderr exactly. Keep JSON reports structured and exclude raw JSON process output.
- Do not add Python, TypeScript, Bun, SnakeYAML, or a second CLI parser.
- Do not mutate Beads or Dolt.
- Commit the complete wave once with Damian Reeves as sole author and committer and no tool attribution.

---

### Task 1: Own the JVM temp probe safely

**Files:**
- Modify: `.claude/skills/squire/SquireEnv.scala`
- Modify: `.claude/skills/squire/SquireTests.scala`

- [x] Add isolated failing tests that prove the predictable probe deletes a legacy sentinel, follows/removes a legacy symlink, and collides when two checks overlap.
- [x] Run the focused Squire suite and retain the real RED results.
- [x] Generate a UUID-suffixed path, open it atomically with `CREATE_NEW`, `WRITE`, and `NOFOLLOW_LINKS`, write through the retained channel, and record ownership only after creation succeeds.
- [x] Close and delete only the owned probe in the finalizer while retaining the existing failure and cleanup reporting.
- [x] Use scoped scratch fixtures and assert no owned probe remains after success or a write failure.
- [x] Re-run the focused suite and retain the GREEN result.

---

### Task 2: Enforce native repeated sparse options

**Files:**
- Modify: `.claude/skills/squire/squire.scala`
- Modify: `.claude/skills/squire/SquireTests.scala`

- [x] Add a real Case App parser-to-handler boundary test for `URL --sparse docs --sparse website --sparse tests/bdd --sparse wit` and assert exact list order.
- [x] In the same boundary fixture, prove `URL accidental --sparse docs`, named plus positional URL duplication, input after `--`, and a missing URL all fail before root discovery, filesystem access, or process execution.
- [x] Prove positional and named URL forms still succeed.
- [x] Run the focused suite and retain the real RED result showing an accidental positional token reached downstream.
- [x] Replace positional sparse continuation with the strict required-argument resolver and pass Case App's decoded sparse list through unchanged.
- [x] Re-run the focused suite and retain the GREEN result.

---

### Task 3: Preserve failed text diagnostics exactly

**Files:**
- Modify: `.claude/skills/squire/SquireSpec.scala`
- Modify: `.claude/skills/squire/SquireTests.scala`

- [x] Add a failing regression with leading indentation, trailing spaces, and trailing newlines in both stdout and stderr.
- [x] Compare exact `SpecStep.detail` and exact rendered report text rather than substring containment.
- [x] Prove JSON-mode detail remains the command only and excludes raw JSON, stdout, and stderr.
- [x] Run the focused suite and retain the real RED result showing `.trim` changed the diagnostic bytes.
- [x] Append complete non-empty streams with only the minimum necessary separator newlines.
- [x] Re-run the focused suite and retain the GREEN result.

---

### Task 4: Synchronize the public contract

**Files:**
- Modify: `.claude/skills/squire/SquireSpec.scala`
- Modify: `.claude/skills/squire/SquireTests.scala`
- Modify: `.claude/skills/squire/references/repo.md`
- Modify: `.claude/skills/squire/references/spec-sync.md`
- Modify: `docs/superpowers/specs/2026-08-10-squire-follow-up-review-fixes-design.md`
- Modify: `docs/superpowers/plans/2026-08-10-squire-follow-up-review-fixes.md`

- [x] Generate the missing-checkout hint with one `--sparse` occurrence per path.
- [x] Update reference documentation and examples to the repeated-flag form and state that unrelated positionals are invalid.
- [x] Update this design and plan to the approved strict contract and all three findings.

---

### Task 5: Verify and deliver the complete wave

- [x] Run `mise run fmt`.
- [x] Run `mise run test:squire`.
- [x] Verify the exact Mill pin.
- [x] Verify no Python or TypeScript files exist under `.claude/skills/squire`.
- [x] Run `git diff --check`.
- [x] Run `mise run lint`.
- [x] Run `mise run ci:local`.
- [x] Self-review the complete diff against the approved contract and base `b5a308fc5d6a023332e20908d9cd417a1d53ca73`.
- [ ] Commit the complete wave once as Damian Reeves, with no attribution trailers.
- [ ] Push `feat/squire-scala-migration` to `origin` without force.
- [ ] Reply to and resolve every addressed actionable review thread.
- [ ] Monitor `gh pr checks 956 --watch --interval 30` to a green terminal state.
- [ ] Fetch review threads again and confirm that none remain unresolved.
