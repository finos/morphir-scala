# Dedicated Squire Policy CI Job Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move Squire and release-policy tests out of GitHub's `lint` job into a required, independently reported `squire-policy` job.

**Architecture:** The workflow will contain two parallel top-level jobs: `lint` runs only formatting checks, while `squire-policy` owns `mise run test:squire`. The Scala Squire CI policy validator will enforce the job boundary, default shallow checkout, lack of upstream job dependencies, unique command placement, and membership in the aggregate `ci` gate.

**Tech Stack:** GitHub Actions YAML, Scala 3, Kyo Test, Mise, Mill

## Global Constraints

- The dedicated job name is exactly `squire-policy`.
- The policy command is exactly `mise run test:squire` and appears exactly once in the workflow.
- The `lint` job contains no Squire invocation.
- `squire-policy` has no `needs` declaration and therefore runs in parallel.
- `squire-policy` uses the default shallow `actions/checkout` behavior; it contains no `fetch-depth` setting.
- The aggregate `ci` job depends on `squire-policy` exactly once.
- The standalone Squire Mill pin remains exactly `1.2.0-RC1-24-042146`.
- Do not add Python, TypeScript, Bun, or another Squire runtime dependency.
- Do not change release behavior, product test selectors, Morphir cache policy, or other CI job dependencies.
- Do not add AI/tool attribution or co-author trailers.

---

### Task 1: Separate Squire policy from lint

**Files:**
- Modify: `.claude/skills/squire/SquireTests.scala:320-337,1083-1103`
- Modify: `.github/workflows/ci.yml:27-51,451-456`

**Interfaces:**
- Consumes: `SquireCiPolicy.inlineList`, `indentedBlock`, `scalar`, `count`, `replaceOnce`, and `rejects`.
- Produces: `assertSquireCiPolicy(workflow: String): Unit`, enforcing the dedicated `squire-policy` job and aggregate dependency.

- [ ] **Step 1: Replace the old placement assertion with the dedicated-job contract**

Update `assertSquireCiPolicy` so it:

```scala
def assertSquireCiPolicy(workflow: String): Unit =
  val jobName  = "squire-policy:"
  val stepName = "Test Squire and release policy"

  expect(
    workflow.linesIterator.count(_ == s"  $jobName") == 1,
    "workflow must contain exactly one squire-policy job"
  )

  val lint = indentedBlock(workflow, "lint:", 2)
  expect(!lint.contains("mise run test:squire"), "lint must not run Squire policy")

  val policy = indentedBlock(workflow, jobName, 2)
  val headers = policy.linesIterator.collect {
    case line if line.startsWith("      - name: ") => line.stripPrefix("      - name: ")
  }.toList
  expect(
    headers == List(
      "Checkout current branch",
      "Setup Scala and Java",
      "Cache scala dependencies",
      "Setup mise",
      stepName
    ),
    s"unexpected squire-policy steps: $headers"
  )
  expect(!policy.linesIterator.exists(_.trim.startsWith("needs:")), "squire-policy must run in parallel")
  expect(!policy.contains("fetch-depth:"), "squire-policy must use the default shallow checkout")

  val step = indentedBlock(policy, s"- name: $stepName", 6)
  expect(scalar(step, "run") == "mise run test:squire", s"$stepName must run test:squire exactly")
  expect(count(workflow, "mise run test:squire") == 1, "workflow must invoke test:squire exactly once")

  val aggregate = inlineList(indentedBlock(workflow, "ci:", 2), "needs")
  expect(aggregate.count(_ == "squire-policy") == 1, "ci must depend on squire-policy exactly once")
```

Rename the test group to `runs Squire policy in a dedicated parallel CI job`. Add mutations covering:

```scala
List(
  "missing job",
  "duplicate job",
  "step moved into lint",
  "step moved into another job",
  "changed command",
  "job dependency added",
  "full-history checkout added",
  "aggregate dependency removed",
  "aggregate dependency duplicated"
)
```

For each mutation, assert `rejects(assertSquireCiPolicy, mutation)`. Construct mutations from the exact `squire-policy` block after GREEN; the initial direct call to `assertSquireCiPolicy(workflow)` supplies the RED against the current workflow.

- [ ] **Step 2: Run the focused suite and verify RED**

Run:

```bash
cd .claude/skills/squire
./mill --no-server --ticker false SquireTests.scala
```

Expected: exit nonzero with `SquireCiPolicySpec` failing because `squire-policy` is absent or `lint` still contains `mise run test:squire`. All compilation must succeed; do not accept a syntax/setup failure as RED.

- [ ] **Step 3: Add the dedicated shallow-checkout workflow job**

Remove `Test Squire and release policy` from `lint`. Add this top-level sibling job immediately after `lint`:

```yaml
  squire-policy:
    runs-on: ubuntu-latest
    timeout-minutes: 30
    steps:
      - name: Checkout current branch
        uses: actions/checkout@v7.0.1
      - name: Setup Scala and Java
        uses: actions/setup-java@v5
        with:
          distribution: "temurin"
          java-version: "25"
      - name: Cache scala dependencies
        uses: coursier/cache-action@v8
      - name: Setup mise
        uses: jdx/mise-action@v4
        with:
          experimental: true
      - name: Test Squire and release policy
        run: mise run test:squire
```

Do not add `needs` or `fetch-depth`. Add `squire-policy` exactly once to the aggregate dependency list:

```yaml
  ci:
    runs-on: ubuntu-latest
    needs: [lint, squire-policy, knowledge-base, test-js, test-jvm, test-native, mill-morphir-unit, mill-morphir-integration, morphir-elm-projects, runtime-generated-fixtures, runtime-tests]
```

- [ ] **Step 4: Run the focused suite and verify GREEN**

Run the same focused command from Step 2.

Expected: exit zero; `SquireCiPolicySpec` passes, including every dedicated-job mutation.

- [ ] **Step 5: Run formatting and unified Squire verification**

Run sequentially with native WSL Mise available on `PATH` and `SQUIRE_MISE_BIN=/home/linuxbrew/.linuxbrew/bin/mise`:

```bash
mise run fmt
mise run test:squire
```

Expected: both exit zero. The Squire CI policy suite must report the dedicated-job group passing.

- [ ] **Step 6: Verify pin, residue, scope, and formatting invariants**

Run:

```bash
test "$(cat .claude/skills/squire/.mill-version)" = "1.2.0-RC1-24-042146"
find .claude/skills/squire -type f \( -name '*.py' -o -name '*.ts' \) -print
git diff --check
git diff --name-only HEAD
```

Expected: pin assertion succeeds, residue search prints nothing, diff check succeeds, and the uncommitted implementation changes are exactly `.claude/skills/squire/SquireTests.scala` and `.github/workflows/ci.yml`; the design and plan documents are already committed.

- [ ] **Step 7: Run repository gates without overlapping Mill builds**

Run sequentially:

```bash
mise run lint
mise run ci:local
```

Expected: both exit zero. `ci:local` continues to run `lint` and `test:squire` as separate local dependencies; this plan changes only their GitHub job boundary.

- [ ] **Step 8: Commit the implementation**

```bash
git add .claude/skills/squire/SquireTests.scala .github/workflows/ci.yml
git diff --cached --check
git commit -m "ci: separate Squire policy from lint"
```

Use the configured Damian Reeves identity with no attribution trailer. If the optional Beads hook alone fails, document it and use the narrow `--no-verify` exception only after all staged and repository gates pass.

- [ ] **Step 9: Review, push, and verify PR checks**

After independent code review is clean:

```bash
git status --short
git push origin feat/squire-scala-migration
gh pr checks 956 --watch --interval 30
gh pr view 956 --json comments,reviews,reviewDecision,mergeStateStatus,statusCheckRollup
```

Expected: clean worktree; normal non-force push; distinct green `lint` and `squire-policy` checks; aggregate `ci` green; publish skipped; no unresolved actionable feedback. Keep PR #956 in draft state unless the human explicitly changes that decision.
