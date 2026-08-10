# Squire Follow-up Review Fixes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the documented multi-path sparse repository command executable and preserve failed `kb check` diagnostics in text-mode spec sync reports.

**Architecture:** Keep Case App's existing command tree, but give `reference repo add` a focused resolver that combines its required URL with variadic sparse continuations from `RemainingArgs`. Keep spec-sync orchestration unchanged except for attaching complete failed text-mode process output to the existing failed check step.

**Tech Stack:** Scala 3, Kyo RC6, kyo-case-app/Case App 2.1.0, kyo-test, Mill `1.2.0-RC1-24-042146`.

## Global Constraints

- Retain the documented `squire reference repo add <url> --sparse PATH [PATH ...]` syntax.
- Preserve named URL and repeated `--sparse` support.
- Reject arguments after `--` and unrelated extra positionals before filesystem or process effects.
- Do not add Python, TypeScript, Bun, SnakeYAML, or a second CLI parser.
- Keep JSON-mode spec reports structured; do not embed their JSON stdout in text detail.
- All commits must name Damian Reeves as the sole human author and committer and contain no tool attribution.

---

### Task 1: Resolve variadic sparse repository paths

**Files:**
- Modify: `.claude/skills/squire/SquireTests.scala:430-500`
- Modify: `.claude/skills/squire/squire.scala:118-152,684-708`

**Interfaces:**
- Consumes: `ReferenceRepoAddOpts`, Case App `RemainingArgs.remaining` and `RemainingArgs.unparsed`.
- Produces: `SquireCli.resolveRepoAddArguments(namedUrl, sparse, positional, unparsed): (String, List[String]) < Abort[SquireError]`.

- [ ] **Step 1: Add the failing resolver regression**

Add a `SquireCliSpec` case that exercises the documented path list and the strict no-sparse boundary:

```scala
"resolves documented sparse continuations and rejects unrelated extras" in {
  val documented = Abort.run[SquireError](
    SquireCli.resolveRepoAddArguments(
      None,
      List("docs"),
      Seq("https://github.com/finos/morphir", "website", "tests/bdd", "wit"),
      Seq.empty
    )
  )
  val unrelated = Abort.run[SquireError](
    SquireCli.resolveRepoAddArguments(None, Nil, Seq("https://example.test/repo", "extra"), Seq.empty)
  )
  for
    documentedResult <- documented
    unrelatedResult  <- unrelated
  yield assert(
    documentedResult == Result.Success(
      "https://github.com/finos/morphir" -> List("docs", "website", "tests/bdd", "wit")
    ) && failureContains(unrelatedResult, "reference repo add", "unexpected positional")
  )
}
```

- [ ] **Step 2: Run the focused suite and verify RED**

Run:

```bash
cd .claude/skills/squire
./mill --no-server --ticker false SquireTests.scala --reporter=console
```

Expected: compilation fails because `resolveRepoAddArguments` is absent, or the new leaf fails while the documented invocation still reports `unexpected positional arguments`.

- [ ] **Step 3: Implement the focused resolver**

Add this API beside the existing generic argument resolvers:

```scala
def resolveRepoAddArguments(
    namedUrl: Option[String],
    sparse: List[String],
    positional: Seq[String],
    unparsed: Seq[String]
): (String, List[String]) < Abort[SquireError] =
  if unparsed.nonEmpty then
    cliFailure("reference repo add", s"arguments after -- are not supported: ${unparsed.mkString(" ")}")
  else
    val (url, remaining) = namedUrl match
      case Some(value) => Some(value) -> positional
      case None        => positional.headOption -> positional.drop(1)
    url match
      case None => cliFailure("reference repo add", "missing required argument <url-or-path>")
      case Some(_) if sparse.isEmpty && remaining.nonEmpty =>
        cliFailure("reference repo add", s"unexpected positional arguments: ${remaining.mkString(" ")}")
      case Some(value) => value -> (sparse ++ remaining)
```

Update `ReferenceRepoAddCmd` to call the new resolver and pass `options.copy(sparse = resolvedSparse)` to `runReferenceAdd`:

```scala
SquireCli.resolveRepoAddArguments(
  options.urlOrPath,
  options.sparse,
  remaining.remaining,
  remaining.unparsed
).flatMap { case (urlOrPath, sparse) =>
  SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
    SquireCli.runReferenceAdd(
      urlOrPath,
      options.copy(sparse = sparse),
      root,
      LiveProcessRunner,
      LiveSquirePlatform,
      java.lang.System.out.print
    )
  }
}
```

- [ ] **Step 4: Run the focused suite and safe parser smoke**

Run the focused suite from Step 2, then:

```bash
./squire reference repo add https://example.invalid/repo --strategy invalid --sparse docs website tests/bdd wit
```

Expected: the suite passes; the smoke reaches the safe `invalid strategy` validation rather than reporting unexpected sparse paths, and performs no clone or fetch.

- [ ] **Step 5: Commit Task 1**

```bash
git add .claude/skills/squire/squire.scala .claude/skills/squire/SquireTests.scala
git commit -m "fix(squire): accept variadic sparse paths"
```

---

### Task 2: Preserve failed text-mode `kb check` diagnostics

**Files:**
- Modify: `.claude/skills/squire/SquireTests.scala:3165-3198,3423-3455`
- Modify: `.claude/skills/squire/SquireSpec.scala:452-467,874-876`

**Interfaces:**
- Consumes: `ProcessResult.stdout`, `ProcessResult.stderr`, `SpecSyncOptions.json`.
- Produces: failed text-mode `SpecStep.detail` containing the command description plus complete non-empty process diagnostics.

- [ ] **Step 1: Extend the fixture and add the failing report regression**

Extend `syncRunner` with exact output controls:

```scala
checkOutput: String = "{\"findings\":[]}",
checkError: Option[String] = None
```

and return them from its check branch:

```scala
ProcessResult(
  request,
  checkExit,
  checkOutput,
  checkError.getOrElse(if checkExit == 0 then "" else "check failed")
)
```

Add this `SquireSpecSpec` leaf:

```scala
"preserves failed text-mode kb check stdout and stderr" in {
  for
    root <- preparedRoot("spec-check-diagnostics")
    report <- SquireSpec.sync(
      SpecSyncOptions(noFetch = true),
      root,
      syncRunner(
        root,
        checkExit = 1,
        checkOutput = "ERROR invalid knowledge entry\n",
        checkError = Some("schema path: kb/example.yaml\n")
      ),
      TestSpecPlatform()
    )
    detail = report.steps.find(_.step == "check").map(_.detail).getOrElse("")
    rendered = SquireSpec.renderText(report)
  yield assert(
    !report.ok && detail.contains("check --no-provenance") &&
      detail.contains("ERROR invalid knowledge entry") &&
      detail.contains("schema path: kb/example.yaml") &&
      rendered.contains("ERROR invalid knowledge entry") &&
      rendered.contains("schema path: kb/example.yaml")
  )
}
```

- [ ] **Step 2: Run the focused suite and verify RED**

Run:

```bash
cd .claude/skills/squire
./mill --no-server --ticker false SquireTests.scala --reporter=console
```

Expected: only the new diagnostic assertions fail because the check step detail is still `check --no-provenance`.

- [ ] **Step 3: Attach complete text diagnostics on failure**

Add a helper that preserves both streams without inventing output:

```scala
private def processDiagnostics(result: ProcessResult): String =
  List(result.stdout.trim, result.stderr.trim).filter(_.nonEmpty).mkString("\n")
```

In `checkStep`, compute the base detail and append diagnostics only for non-JSON failures:

```scala
val baseDetail = arguments.mkString(" ")
val diagnostics = if process.exitCode != 0 && !json then processDiagnostics(process) else ""
val detail = if diagnostics.nonEmpty then s"$baseDetail\n$diagnostics" else baseDetail
SpecReport(
  "spec-sync",
  ok = process.exitCode == 0,
  steps :+ SpecStep("check", status, detail, result = result)
)
```

- [ ] **Step 4: Run the focused suite and verify GREEN**

Run the focused command from Step 2.

Expected: all Squire suites pass, including the text diagnostic regression and the existing typed JSON reporting tests.

- [ ] **Step 5: Commit Task 2**

```bash
git add .claude/skills/squire/SquireSpec.scala .claude/skills/squire/SquireTests.scala
git commit -m "fix(squire): preserve kb check diagnostics"
```

---

### Task 3: Complete verification and PR review lifecycle

**Files:**
- Verify: `.claude/skills/squire/.mill-version`
- Verify: `.claude/skills/squire/`
- Verify: PR #956 review threads and checks

**Interfaces:**
- Consumes: the two focused commits from Tasks 1 and 2.
- Produces: a pushed, green PR with the two actionable follow-up threads replied to and resolved.

- [ ] **Step 1: Format and run the full Squire gate**

```bash
mise run fmt
mise run test:squire
```

Expected: both commands exit 0 and the complete Squire/Kyo suite passes.

- [ ] **Step 2: Verify constraints and lint**

```bash
test "$(cat .claude/skills/squire/.mill-version)" = "1.2.0-RC1-24-042146"
test -z "$(find .claude/skills/squire -type f \( -name '*.py' -o -name '*.ts' \) -print -quit)"
git diff --check
mise run lint
```

Expected: every command exits 0; the pin is unchanged and no Python or TypeScript Squire files exist.

- [ ] **Step 3: Run local aggregate CI**

```bash
mise run ci:local
```

Expected: exit 0 with `ci:local complete`.

- [ ] **Step 4: Push the existing PR branch**

```bash
git push origin feat/squire-scala-migration
```

Expected: the remote head advances to the two fix commits and PR #956 remains ready for review.

- [ ] **Step 5: Reply to and resolve the actionable threads**

Reply in each inline thread with the focused commit SHA and verification evidence, then resolve:

- `PRRT_kwDOHZmaWc6XwiEQ` — variadic sparse paths.
- `PRRT_kwDOHZmaWc6XwiES` — retained `kb check` diagnostics.

Do not reopen the resolved authorship thread unless GitHub metadata changes.

- [ ] **Step 6: Monitor hosted checks and review feedback**

```bash
gh pr checks 956 --watch --interval 30
python3 /mnt/c/Users/damre/.codex/skills/gh-address-comments/scripts/fetch_comments.py
```

Expected: all required checks pass and no unresolved review thread remains. If a new thread appears, verify it against the branch before implementing or resolving it.
