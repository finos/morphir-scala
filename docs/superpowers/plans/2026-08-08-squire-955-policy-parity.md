# Squire PR #955 Policy Parity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Preserve every Squire runtime and policy regression introduced by PR #955 in the unified Scala/Kyo suite.

**Architecture:** Extend the existing pure policy helpers in `SquireTests.scala`, exercise the real workflow/task/manifests with literal mutation tables, and add filesystem-backed doctor edge coverage. Production Squire code changes only when a new doctor regression demonstrates a missing behavior.

**Tech Stack:** Scala 3.8.4, Kyo Test RC6, Mill single-file modules, Mise, GitHub Actions YAML inspected through existing text-block helpers.

## Global Constraints

- Keep `.claude/skills/squire/.mill-version` exactly `1.2.0-RC1-24-042146`.
- Add no Python, TypeScript, Bun runtime, YAML parser, JSON parser, or workflow parser dependency to Squire.
- Use `final case class` for any new concrete case class.
- Preserve human-only FINOS commit authorship and omit tool attribution.
- Do not change publication permissions, commit/push behavior, or Beads/Dolt state.

---

### Task 1: Hosted CI policy parity

**Files:**
- Modify: `.claude/skills/squire/SquireTests.scala:40-181`
- Modify: `.claude/skills/squire/SquireTests.scala:624-766`

**Interfaces:**
- Consumes: `SquireCiPolicy.indentedBlock`, `inlineList`, `scalar`, `replaceOnce`, `replaceInJob`, and `rejects`.
- Produces: `assertReadOnlyPermissions`, `assertMorphirCapabilityPolicy`, `assertJvmPlatformPolicy`, and `assertMorphirCachePolicy` pure validators.

- [ ] **Step 1: Add failing permission and capability mutation tests**

Add tests under `SquireCiPolicySpec` that call the wished-for validators on the real `.github/workflows/ci.yml`, root `build.mill`, and `.config/mise/tasks/test/jvm-platform`. Use literal mutations for:

```scala
val permissionMutations = List(
  replaceOnce(workflow, "permissions:\n  contents: read", "permissions:\n  contents: write"),
  replaceOnce(workflow, "permissions:\n  contents: read", "permissions:\n  contents: read\n  packages: write")
)

val capabilityMutations = List(
  replaceInJob(workflow, "mill-morphir-unit:",
    "'mill-plugins.morphir.{toolchain,javascript,elm-tooling,core,elm}.__.test'",
    "'mill-plugins.morphir.__.test'"),
  replaceInJob(workflow, "test-jvm:", "mise run test:jvm-platform", "mise run test:jvm"),
  replaceInJob(workflow, "mill-morphir-integration:", "needs: [mill-morphir-unit]", "needs: []"),
  replaceInJob(workflow, "runtime-tests:", "needs: [runtime-generated-fixtures]", "needs: [test-jvm]")
)
```

Load `build.mill` and the JVM-platform task as real fixtures. Mutate the alias to add `morphir.runtime.classic.jvm.test` and mutate the task away from `Alias/run testJVMPlatform`. Assert every mutation is rejected.

- [ ] **Step 2: Run focused tests and record RED**

Run:

```bash
cd .claude/skills/squire
./mill --no-server --ticker false SquireTests.scala
```

Expected: compilation fails because the four validators do not exist, or the new mutation assertions fail against the current under-constrained policy.

- [ ] **Step 3: Implement the minimal hosted-CI validators**

Add pure helpers to `SquireCiPolicy`:

```scala
def assertReadOnlyPermissions(workflow: String): Unit =
  val permissions = indentedBlock(workflow, "permissions:", 0)
  expect(permissions.linesIterator.filter(_.trim.nonEmpty).toList == List("  contents: read"),
    "workflow permissions must be exactly contents: read")

def assertMorphirCapabilityPolicy(workflow: String): Unit =
  val commands = List(
    "mill-morphir-unit:" -> "'mill-plugins.morphir.{toolchain,javascript,elm-tooling,core,elm}.__.test'",
    "mill-morphir-integration:" -> "mill-plugins.morphir.integration.test",
    "morphir-elm-projects:" -> "examples.morphir-elm-projects.__.morphirIR",
    "runtime-generated-fixtures:" -> "morphir.runtime.classic.jvm.test.generatedRuntimeFixtures",
    "runtime-tests:" -> "morphir.runtime.classic.jvm.test.verifyRuntimeTestDiscovery"
  )
  commands.foreach { case (job, command) =>
    val block = indentedBlock(workflow, job, 2)
    expect(block.contains(command), s"$job must run $command")
  }
  val unit = indentedBlock(workflow, "mill-morphir-unit:", 2)
  expect(!unit.contains("mill-plugins.morphir.integration"), "unit selector must exclude integration")
  List(
    "mill-morphir-integration:" -> "[mill-morphir-unit]",
    "morphir-elm-projects:" -> "[mill-morphir-unit]",
    "runtime-generated-fixtures:" -> "[morphir-elm-projects]",
    "runtime-tests:" -> "[runtime-generated-fixtures]"
  ).foreach { case (job, dependency) =>
    expect(scalar(indentedBlock(workflow, job, 2), "needs") == dependency,
      s"$job must depend on $dependency")
  }

def assertJvmPlatformPolicy(workflow: String, buildMill: String, task: String): Unit =
  expect(indentedBlock(workflow, "test-jvm:", 2).contains("run: mise run test:jvm-platform"),
    "generic JVM CI must use test:jvm-platform")
  expect(task.linesIterator.map(_.trim).contains("./mill -i Alias/run testJVMPlatform"),
    "test:jvm-platform must invoke Alias/run testJVMPlatform")
  val aliasStart = buildMill.indexOf("def testJVMPlatform")
  val aliasEnd   = buildMill.indexOf("def testJVMCached", aliasStart)
  expect(aliasStart >= 0 && aliasEnd > aliasStart, "missing testJVMPlatform alias")
  val alias = buildMill.substring(aliasStart, aliasEnd)
  List(
    "morphir.jvm.__.compile",
    "morphir.{contrib.knowledge,extensibility,intelligence.sdk,interop.borer,interop.zio.json,kit.kyo,langkit.core,langkit.elm.compiler.api,langkit.elm.core,langkit.trees,lib.interop,model,model.lowering,naming,testing.generators,testing.zio,tests,tools}.jvm.__.compile",
    "morphir.{contrib.knowledge,intelligence.sdk,interop.borer,interop.zio.json,kit.kyo,langkit.core,langkit.elm.compiler.api,langkit.elm.core,langkit.trees,model,model.lowering,tests}.jvm.test",
    "morphir.langkit.itest.testCached"
  ).foreach(selector => expect(alias.contains(selector), s"JVM alias missing $selector"))
  expect(!alias.contains("morphir.runtime.classic"), "JVM platform alias must exclude classic runtime")

def assertMorphirCachePolicy(workflow: String): Unit =
  List("mill-morphir-unit:", "mill-morphir-integration:", "morphir-elm-projects:",
    "runtime-generated-fixtures:", "runtime-tests:").foreach { job =>
    expect(indentedBlock(workflow, job, 2).contains("path: ~/.cache/morphir-scala"),
      s"$job must cache verified Morphir downloads")
  }
  val unit = indentedBlock(workflow, "mill-morphir-unit:", 2)
  expect(unit.contains("out/mill-plugins/morphir/") &&
    unit.contains("!out/mill-plugins/morphir/**/testForked.dest/**") &&
    unit.contains("!out/mill-plugins/morphir/**/testOnly.dest/**"),
    "Mill unit cache must contain only reusable capability outputs")
  val projects = indentedBlock(workflow, "morphir-elm-projects:", 2)
  expect(projects.contains("out/examples/morphir-elm-projects/") && projects.contains("out/morphir-elm/"),
    "project cache must contain generated IR outputs")
  List("runtime-generated-fixtures:", "runtime-tests:").foreach { job =>
    expect(indentedBlock(workflow, job, 2).contains("out/morphir/runtime/classic/jvm/test/"),
      s"$job must use the classic runtime fixture output cache")
  }
```

Use literal expected selectors and paths derived from PR #955, not values generated from the workflow under test. Keep failure messages specific to the violated contract.

- [ ] **Step 4: Add cache-path mutation coverage**

Mutate each Morphir cache block independently: replace `~/.cache/morphir-scala`, broaden `out/mill-plugins/morphir/` to `out/`, remove the test-fork exclusions, broaden generated-project outputs, and move classic runtime output into a pre-fixture job. Verify `assertMorphirCachePolicy` rejects every mutation.

- [ ] **Step 5: Run focused tests and verify GREEN**

Run the same focused command. Expected: `SquireCiPolicySpec` and all existing suites pass.

- [ ] **Step 6: Commit the hosted-CI parity group**

```bash
git add .claude/skills/squire/SquireTests.scala
git commit -m "test(squire): preserve PR 955 CI policy"
```

---

### Task 2: Mise and Morphir Elm provisioning parity

**Files:**
- Modify: `.claude/skills/squire/SquireTests.scala:768-807`

**Interfaces:**
- Consumes: repository paths rooted from `SquireMisePolicySpec.repositoryRoot` and the existing `LiveProcessRunner` metadata check.
- Produces: `assertMiseMorphirPolicy(files: Map[String, String]): Unit`, a pure test helper accepting complete real file contents.

- [ ] **Step 1: Add failing repository-policy tests**

Load these real files into a complete fixture map:

```text
.config/mise/tasks/build/elm
.config/mise/tasks/build/morphir-elm
.config/mise/tasks/ci/local
.config/mise/tasks/setup
package.json
examples/morphir-elm-projects/defaults-tests/package.json
examples/morphir-elm-projects/evaluator-tests/package.json
examples/morphir-elm-projects/finance/package.json
examples/morphir-elm-projects/unit-test-framework/example-project/package.json
examples/morphir-elm-projects/unit-test-framework/example-project-tests/package.json
examples/morphir-elm-projects/unit-test-framework/example-project-tests-passing/package.json
examples/morphir-elm-projects/unit-test-framework/example-project-tests-incomplete/package.json
```

Call the wished-for `assertMiseMorphirPolicy`. Add literal mutations that introduce `bun install` without `--ignore-scripts`, add a `morphir-elm` development dependency, add a `make` script, make either build wrapper run Bun/npm, remove a required Mill selector, or collapse dedicated local-CI steps.

- [ ] **Step 2: Run focused tests and record RED**

Run the focused Squire command. Expected: compilation fails because `assertMiseMorphirPolicy` does not exist.

- [ ] **Step 3: Implement the minimal Mise policy helper**

Validate observable repository contracts from the supplied file map:

- `build/elm` invokes only the two approved Morphir IR selectors after shell boilerplate and human-readable echoes;
- `build/morphir-elm` invokes only `examples.morphir-elm-projects.evaluator-tests.morphirIR`;
- `ci/local` contains separate unit, integration, generated project, fixture, discovery, and runtime invocations in order;
- setup contains exactly one `bun install --ignore-scripts` and no plain `bun install` command;
- every package manifest lacks a `morphir-elm` dependency and a `make` script.

Use narrow regular expressions or the existing count helper. Do not introduce production code or parse JSON with the implementation under test.

Implement the helper with literal contracts:

```scala
def assertMiseMorphirPolicy(files: Map[String, String]): Unit =
  def content(path: String): String = files.getOrElse(path, throw new AssertionError(s"missing fixture: $path"))
  val buildElm = content(".config/mise/tasks/build/elm")
  assert(buildElm.contains("examples.morphir-elm-projects.__.morphirIR"))
  assert(buildElm.contains("morphir-elm.sdks.__.morphirIR"))
  assert(!List("bun ", "npm ", "npx ").exists(buildElm.contains))

  val buildEvaluator = content(".config/mise/tasks/build/morphir-elm")
  assert(buildEvaluator.contains("examples.morphir-elm-projects.evaluator-tests.morphirIR"))
  assert(!List("bun ", "npm ", "npx ").exists(buildEvaluator.contains))

  val localCi = content(".config/mise/tasks/ci/local")
  List(
    "mill-plugins.morphir.{toolchain,javascript,elm-tooling,core,elm}.__.test",
    "mill-plugins.morphir.integration.test",
    "examples.morphir-elm-projects.__.morphirIR",
    "morphir.runtime.classic.jvm.test.generatedRuntimeFixtures",
    "morphir.runtime.classic.jvm.test.verifyRuntimeTestDiscovery",
    "morphir.runtime.classic.jvm.test"
  ).foreach(selector => assert(localCi.contains(selector)))

  val setup = content(".config/mise/tasks/setup")
  assert(SquireCiPolicy.count(setup, "bun install --ignore-scripts") == 1)
  assert(!setup.linesIterator.exists(line => line.trim == "bun install"))

  files.iterator.filter { case (path, _) => path.endsWith("package.json") }.foreach { case (path, json) =>
    assert(!json.contains("\"morphir-elm\""), s"$path must not install morphir-elm")
    assert("(?s)\"scripts\"\\s*:\\s*\\{.*?\"make\"\\s*:".r.findFirstIn(json).isEmpty,
      s"$path must not define a make script")
  }
```

- [ ] **Step 4: Run focused tests and verify GREEN**

Expected: `SquireMisePolicySpec` and all existing suites pass.

- [ ] **Step 5: Commit the Mise parity group**

```bash
git add .claude/skills/squire/SquireTests.scala
git commit -m "test(squire): preserve PR 955 Mise policy"
```

---

### Task 3: Doctor acquisition-cache edge parity

**Files:**
- Modify: `.claude/skills/squire/SquireTests.scala:3728-3848`
- Modify only if a demonstrated gap exists: `.claude/skills/squire/SquireDoctor.scala:102-182`

**Interfaces:**
- Consumes: `SquireDoctor.run`, `SquireFixtures.scratch`, and injected `SquireEnv.Platform.environment`.
- Produces: parameterized filesystem-backed coverage for all #955 cache diagnostic outcomes.

- [ ] **Step 1: Add cache mode and bound tests**

Add cases proving:

- disabled mode reports non-blocking `DISABLED`, does not inspect corrupt content, and still rejects a relative `MORPHIR_NODE_CACHE`;
- 257 directory entries report a non-blocking `NOTICE` containing `directory entry limit reached (256)`;
- an unreadable, replaced, or non-regular digest entry is either blocking `CORRUPT` when structurally invalid or a bounded non-blocking notice when it cannot be read safely;
- enough valid entries to exceed the 256 MiB total hash budget produce a non-blocking `NOTICE` rather than `CORRUPT`.

Use sparse files for byte-bound cases and always restore permissions/clean resources in `finally` blocks.

- [ ] **Step 2: Prove the new tests catch realistic mutations**

Because most required behavior already exists, run each new test group against one temporary production mutation at a time:

- move the relative-path check below disabled mode;
- change `CacheDiagnosticMaxEntries` from `256` to `257`;
- remove the unreadable-entry notice branch;
- remove the total hash budget branch.

Expected: the corresponding new test fails for each mutation. Restore the production source exactly after every proof and confirm `git diff` contains only the intended tests.

- [ ] **Step 3: Fix only demonstrated production gaps**

If an unmutated test fails, make the smallest change in `SquireDoctor.scala` that restores the #955 behavior while retaining `NOFOLLOW_LINKS`, the 64 MiB entry bound, 256-entry bound, and 256 MiB total hash bound.

- [ ] **Step 4: Run focused tests and verify GREEN**

Expected: `SquireDoctorSpec` and all existing suites pass with no probe files left behind.

- [ ] **Step 5: Commit the doctor parity group**

```bash
git add .claude/skills/squire/SquireTests.scala .claude/skills/squire/SquireDoctor.scala
git commit -m "test(squire): preserve PR 955 cache diagnostics"
```

---

### Task 4: Parity matrix and completion gates

**Files:**
- Modify: `docs/superpowers/specs/2026-08-08-squire-955-policy-parity-design.md`
- Verify: `.claude/skills/squire/.mill-version`

**Interfaces:**
- Consumes: the 31 Python test names added by `c331b2cd^..c331b2cd` and the final Scala test names.
- Produces: a reviewable one-to-one parity table.

- [ ] **Step 1: Append the parity matrix**

Add a table with one row for each PR #955 Python test name and the exact Scala suite/test group that protects it. Mark the missing-Java temp diagnostic as deliberately inapplicable only if justified: the unified Squire launcher itself requires a JVM, while missing/absent `java.io.tmpdir` remains covered at the Scala platform boundary.

- [ ] **Step 2: Run formatting and the focused unified suite**

```bash
mise run fmt
mise run test:squire
```

Expected: all Squire/Kyo suites and snapshot policy tests pass.

- [ ] **Step 3: Verify migration and pin invariants**

```bash
test "$(cat .claude/skills/squire/.mill-version)" = "1.2.0-RC1-24-042146"
find .claude/skills/squire -type f \( -name '*.py' -o -name '*.ts' \) -print
git diff --check
```

Expected: pin assertion succeeds, the legacy-file search prints nothing, and the diff check succeeds.

- [ ] **Step 4: Run repository gates**

```bash
mise run lint
mise run ci:local
```

Expected: both commands exit zero. Do not overlap Mill builds that share `out/`.

- [ ] **Step 5: Commit the parity matrix and any formatting-only changes**

```bash
git add docs/superpowers/specs/2026-08-08-squire-955-policy-parity-design.md .claude/skills/squire/SquireTests.scala .claude/skills/squire/SquireDoctor.scala
git commit -m "docs(squire): record PR 955 Scala parity"
```

- [ ] **Step 6: Review, push, and monitor the existing PR**

```bash
git status --short
git push origin feat/squire-scala-migration
gh pr checks 956 --watch --interval 30
gh pr view 956 --json comments,reviews,reviewDecision,mergeStateStatus,statusCheckRollup
```

Expected: clean worktree, branch pushed without force, all required checks green, and no unresolved actionable feedback.
