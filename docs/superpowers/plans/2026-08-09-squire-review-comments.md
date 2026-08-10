# Squire Review-Comment Fixes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make every documented Squire positional command work through Kyo Case App and make doctor inspect the same platform-specific acquisition-cache root as the Mill toolchain.

**Architecture:** Preserve the existing multi-token `KyoCommand` hierarchy. Command handlers consume Case App `RemainingArgs`, resolve named-or-positional values through one strict CLI helper, and route only fully validated values to the existing typed operations. Add normalized operating-system predicates to `SquireEnv.Platform`; doctor mirrors `AcquisitionSettings.defaultCacheRoot` without depending on the Mill plugin module.

**Tech Stack:** Scala 3.8.4, Kyo Case App 1.0.0-RC6, Case App 2.1.0, Kyo Test, Mill 1.2.0-RC1-24-042146, Mise.

## Global Constraints

- Keep `.claude/skills/squire/.mill-version` exactly `1.2.0-RC1-24-042146`.
- Keep Squire free of Python, TypeScript, Bun, and SnakeYAML runtime dependencies.
- Keep the existing multi-token command paths; do not add nested `CommandsEntryPoint` objects.
- Retain named-option forms while making the documented positional forms authoritative.
- Reject missing, duplicate, excess, and post-`--` values before process or filesystem work.
- Derive `isWindows`, `isMacOS`, and `isLinux` from one normalized operating-system name.
- Mirror `AcquisitionSettings.defaultCacheRoot` for macOS, Windows, and XDG-compatible systems.
- Preserve the unified `SquireError` and launcher exit-code boundary.
- Do not mutate Beads/Dolt state, reply to GitHub comments, resolve review threads, mark the PR ready, or push until explicitly authorized by the user.

---

### Task 1: Native Case App positional arguments

**Files:**
- Modify: `.claude/skills/squire/squire.scala:20-56,112-270,562-666`
- Test: `.claude/skills/squire/SquireTests.scala:387-525`

**Interfaces:**
- Consumes: `caseapp.RemainingArgs`, existing `SquireCli.runCellar`, and existing reference-repository routing methods.
- Produces: `SquireCli.resolveRequiredArguments(command, fields, positional, unparsed)`, `SquireCli.resolveOptionalArgument(command, field, named, positional, unparsed)`, `runReferenceAdd(urlOrPath, options, ...)`, `runReferenceStatus(name, ...)`, and `runReferenceRemove(name, keepFiles, ...)`; updated Kyo command handlers that accept `RemainingArgs`.

- [ ] **Step 1: Add parser regressions for every documented positional command**

Add a `SquireCliSpec` group that calls each command's `parser.detailedParse` with the documented values and proves Case App leaves them in `RemainingArgs.remaining` rather than rejecting the command:

```scala
"accepts every documented positional command through RemainingArgs" in {
  val cases = List(
    SquireApp.CellarGetCmd.parser.detailedParse(Seq("org.example:demo:1", "demo.Symbol")) -> List("org.example:demo:1", "demo.Symbol"),
    SquireApp.CellarSearchCmd.parser.detailedParse(Seq("org.example:demo:1", "Symbol")) -> List("org.example:demo:1", "Symbol"),
    SquireApp.CellarDepsCmd.parser.detailedParse(Seq("org.example:demo:1")) -> List("org.example:demo:1"),
    SquireApp.ReferenceRepoAddCmd.parser.detailedParse(Seq("https://example.test/repo")) -> List("https://example.test/repo"),
    SquireApp.ReferenceRepoStatusCmd.parser.detailedParse(Seq("mill")) -> List("mill"),
    SquireApp.ReferenceRepoRemoveCmd.parser.detailedParse(Seq("mill")) -> List("mill")
  )
  assert(cases.forall { case (Right((_, remaining)), expected) => remaining.remaining == expected; case _ => false })
}
```

The option case classes still contain required `String` fields, so this test must initially fail on Case App missing-option errors.

- [ ] **Step 2: Run the focused suite and record RED**

Run:

```bash
cd .claude/skills/squire
./mill --no-server --ticker false SquireTests.scala --reporter=console
```

Expected: `SquireCliSpec` fails because positional invocations still require `--coordinate`, `--symbol`, `--url-or-path`, or `--name`.

- [ ] **Step 3: Add strict argument-resolution regressions**

Add tests for the wished-for helper API. Prove ordered hybrid resolution and fail-closed behavior without invoking a runner:

```scala
val mixed = Abort.run[SquireError](
  SquireCli.resolveRequiredArguments(
    "cellar get",
    List("coordinate" -> Some("org.example:demo:1"), "symbol" -> None),
    Seq("demo.Symbol"),
    Seq.empty
  )
)
val duplicate = Abort.run[SquireError](
  SquireCli.resolveRequiredArguments(
    "cellar deps",
    List("coordinate" -> Some("org.example:demo:1")),
    Seq("other:coordinate:1"),
    Seq.empty
  )
)
val missing = Abort.run[SquireError](
  SquireCli.resolveRequiredArguments("reference repo remove", List("name" -> None), Seq.empty, Seq.empty)
)
val afterDoubleDash = Abort.run[SquireError](
  SquireCli.resolveRequiredArguments("cellar deps", List("coordinate" -> None), Seq.empty, Seq("org.example:demo:1"))
)
```

Assert that `mixed` succeeds with the ordered two values, while duplicate, missing, excess, and post-`--` cases are `Result.Failure` values whose messages name the command and offending argument condition. Cover the optional zero-or-one boundary explicitly:

```scala
val absentOptional = Abort.run[SquireError](
  SquireCli.resolveOptionalArgument("reference repo status", "name", None, Seq.empty, Seq.empty)
)
val positionalOptional = Abort.run[SquireError](
  SquireCli.resolveOptionalArgument("reference repo status", "name", None, Seq("mill"), Seq.empty)
)
val duplicateOptional = Abort.run[SquireError](
  SquireCli.resolveOptionalArgument("reference repo status", "name", Some("mill"), Seq("other"), Seq.empty)
)
```

Assert `Result.Success(None)`, `Result.Success(Some("mill"))`, and `Result.Failure`, respectively.

Also assert generated help renders the positional contract rather than presenting those values as mandatory flags:

```scala
val helpFormat = caseapp.core.help.HelpFormat.default(ansiColors = false)
val cellarHelp = SquireApp.CellarGetCmd.finalHelp.withProgName("squire cellar get").help(helpFormat, showHidden = false)
val addHelp    = SquireApp.ReferenceRepoAddCmd.finalHelp.withProgName("squire reference repo add").help(helpFormat, showHidden = false)
assert(cellarHelp.contains("<coordinate> <symbol>") && addHelp.contains("<url-or-path>"))
```

- [ ] **Step 4: Run the focused suite and confirm the helper RED is a missing-symbol failure**

Run the same Mill command. Expected: compilation fails only because `resolveRequiredArguments` and `resolveOptionalArgument` do not exist.

- [ ] **Step 5: Make positional-capable fields optional and document usage**

Change the six option models to optional named fields and annotate their remaining-argument display:

```scala
@ArgsName("<coordinate> <symbol>")
final case class CellarGetOpts(
    coordinate: Option[String] = None,
    symbol: Option[String] = None,
    hideInherited: Boolean = false,
    groupInherited: Boolean = false,
    limit: Option[Int] = None,
    tempDirectory: Option[String] = None
)

@ArgsName("<url-or-path>")
final case class ReferenceRepoAddOpts(
    urlOrPath: Option[String] = None,
    name: Option[String] = None,
    ref: Option[String] = None,
    strategy: String = "clone",
    depth: Option[Int] = None,
    full: Boolean = false,
    sparse: List[String] = Nil
)
```

Apply matching annotations and optional fields to Cellar search/deps and reference status/remove.

- [ ] **Step 6: Implement the minimal strict resolvers**

Add pure validation at the top of `SquireCli`'s command-routing boundary:

```scala
private def cliFailure[A](command: String, message: String): A < Abort[SquireError] =
  Abort.fail(SquireError.Failure("cli", s"$command: $message"))

def resolveRequiredArguments(
    command: String,
    fields: List[(String, Option[String])],
    positional: Seq[String],
    unparsed: Seq[String]
): List[String] < Abort[SquireError] =
  if unparsed.nonEmpty then cliFailure(command, s"arguments after -- are not supported: ${unparsed.mkString(" ")}")
  else
    val remaining = scala.collection.mutable.Queue.from(positional)
    val resolved  = fields.map { case (name, named) => name -> named.orElse(remaining.dequeueFirst(_ => true)) }
    resolved.collectFirst { case (name, None) => name } match
      case Some(name) => cliFailure(command, s"missing required argument <$name>")
      case None if remaining.nonEmpty => cliFailure(command, s"unexpected positional arguments: ${remaining.mkString(" ")}")
      case None => resolved.flatMap(_._2)

def resolveOptionalArgument(
    command: String,
    field: String,
    named: Option[String],
    positional: Seq[String],
    unparsed: Seq[String]
): Option[String] < Abort[SquireError] =
  if unparsed.nonEmpty then cliFailure(command, s"arguments after -- are not supported: ${unparsed.mkString(" ")}")
  else
    val resolved = named.orElse(positional.headOption)
    val consumed = if named.isDefined then 0 else math.min(1, positional.size)
    val extra    = positional.drop(consumed)
    if extra.nonEmpty then cliFailure(command, s"unexpected positional arguments: ${extra.mkString(" ")}")
    else resolved
```

The optional resolver allows zero or one resolved value. Both helpers reject every unconsumed token and share the same `SquireError` rendering boundary.

- [ ] **Step 7: Route all six commands through the Kyo two-argument overload**

Change each command to `run { (options, remaining) => ... }`. Resolve its fields before finding the repository root or invoking a runner:

```scala
run { (options, remaining) =>
  SquireCli.runCommand(
    SquireCli.resolveRequiredArguments(
      "cellar get",
      List("coordinate" -> options.coordinate, "symbol" -> options.symbol),
      remaining.remaining,
      remaining.unparsed
    ).flatMap { values =>
      val coordinate :: symbol :: Nil = values: @unchecked
      SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
        SquireCli.runCellar(
          CellarAction.Get(coordinate, symbol, options.hideInherited, options.groupInherited, options.limit),
          root,
          LiveProcessRunner,
          LiveSquirePlatform,
          java.lang.System.out.print,
          java.lang.System.err.print,
          options.tempDirectory
        )
      }
    }
  )
}
```

Use the same boundary for Cellar search/deps and reference add/remove. For reference status, pass the resolved optional name to the updated routing method.

Update reference routing signatures so optional parser fields never leak into the domain layer:

```scala
def runReferenceAdd(urlOrPath: String, options: ReferenceRepoAddOpts, root: Path, runner: ProcessRunner, platform: SquirePlatform, output: String => Unit): Int < (Async & Sync & Abort[SquireError])
def runReferenceStatus(name: Option[String], root: Path, runner: ProcessRunner, output: String => Unit): Int < (Async & Sync & Abort[SquireError])
def runReferenceRemove(name: String, keepFiles: Boolean, root: Path, runner: ProcessRunner, platform: SquirePlatform, output: String => Unit): Int < (Async & Sync & Abort[SquireError])
```

Construct `ReferenceAdd(urlOrPath, options.name, options.ref, options.strategy, options.depth, options.full, options.sparse)` only after `urlOrPath` is resolved.

- [ ] **Step 8: Verify routing has no side effects on argument errors**

Prove validation happens before downstream work by composing each failing resolver with a guarded effect:

```scala
var downstreamInvoked = false
val result = Abort.run[SquireError](
  SquireCli.resolveRequiredArguments(
    "reference repo remove",
    List("name" -> Some("mill")),
    Seq("duplicate"),
    Seq.empty
  ).flatMap(_ => Sync.defer(downstreamInvoked = true))
)
result.map(outcome => assert(outcome.isFailure && !downstreamInvoked))
```

Repeat for missing, excess, and post-`--` inputs. This establishes that command handlers can place root discovery, runners, and filesystem work strictly after successful resolution.

- [ ] **Step 9: Run GREEN and inspect generated help**

Run the focused suite, then:

```bash
.claude/skills/squire/squire cellar get --help
.claude/skills/squire/squire reference repo add --help
```

Expected: the suite passes and usage text includes `<coordinate> <symbol>` and `<url-or-path>` without launcher stack traces or missing-option errors.

- [ ] **Step 10: Commit Task 1**

```bash
git add .claude/skills/squire/squire.scala .claude/skills/squire/SquireTests.scala
git commit -m "fix(squire): accept documented positional arguments"
```

---

### Task 2: Platform predicates and acquisition-cache parity

**Files:**
- Modify: `.claude/skills/squire/SquireEnv.scala:40-78`
- Modify: `.claude/skills/squire/SquireDoctor.scala:102-118`
- Test: `.claude/skills/squire/SquireTests.scala:3126-3150,4680-5000,5187-5210`

**Interfaces:**
- Consumes: `SquireEnv.Platform.environment`, `SquireEnv.Platform.home`, and the current cache-inspection safety limits.
- Produces: `Platform.osName`, final predicates `isWindows`, `isMacOS`, `isLinux`, and doctor default-cache selection equivalent to `AcquisitionSettings.defaultCacheRoot`.

- [ ] **Step 1: Write RED predicate tests**

Extend `TestEnvPlatform` and `SquireFixtures.platform` calls in the test only with a wished-for `osName` input. Add exact predicate assertions:

```scala
val windows = SquireFixtures.platform(root, ok, osName = "Windows 11")
val mac     = SquireFixtures.platform(root, ok, osName = "Mac OS X")
val darwin  = SquireFixtures.platform(root, ok, osName = "Darwin")
val linux   = SquireFixtures.platform(root, ok, osName = "Linux")
assert(
  windows.isWindows && !windows.isMacOS && !windows.isLinux &&
    mac.isMacOS && darwin.isMacOS && !mac.isWindows &&
    linux.isLinux && !linux.isWindows && !linux.isMacOS
)
```

- [ ] **Step 2: Run the focused suite and record the compile RED**

Run the Squire Mill command. Expected: compilation fails because `Platform.osName`, `isWindows`, `isMacOS`, and `isLinux` are absent.

- [ ] **Step 3: Implement normalized platform predicates**

Add the platform API:

```scala
trait Platform:
  def osName: String
  private final def normalizedOsName: String = osName.toLowerCase(java.util.Locale.ROOT)
  final def isWindows: Boolean = normalizedOsName.startsWith("windows")
  final def isMacOS: Boolean   = normalizedOsName.startsWith("mac") || normalizedOsName == "darwin"
  final def isLinux: Boolean   = normalizedOsName.startsWith("linux")
```

`LivePlatform.osName` returns `System.getProperty("os.name", "")`. Add `osName: String = "Linux"` to `SquireFixtures.platform` and `TestEnvPlatform` so existing tests retain their current XDG behavior.

- [ ] **Step 4: Run GREEN for predicate tests**

Run the focused suite. Expected: all existing tests and the new predicate group pass.

- [ ] **Step 5: Write filesystem-backed RED tests for platform cache roots**

For each platform, create a valid 64-hex filename containing deliberately mismatched bytes at the expected cache root's `sha256` directory:

```scala
def corrupt(root: Path): Unit = {
  val digestRoot = root / "sha256"
  Files.createDirectories(digestRoot.toJava)
  Files.writeString((digestRoot / ("0" * 64)).toJava, "not the zero digest")
}
```

Cover this table:

| OS | Environment | Expected root |
| --- | --- | --- |
| `Mac OS X` | `XDG_CACHE_HOME=/ignored` | `home/Library/Caches/morphir-scala` |
| `Windows 11` | absolute `LOCALAPPDATA` | `LOCALAPPDATA/morphir-scala/Cache` |
| `Windows 11` | relative `LOCALAPPDATA` | `home/AppData/Local/morphir-scala/Cache` |
| `Linux` | absolute `XDG_CACHE_HOME` | `XDG_CACHE_HOME/morphir-scala` |
| `Linux` | relative `XDG_CACHE_HOME` | `home/.cache/morphir-scala` |

Run `SquireDoctor.run` for each and assert the `acquisition_cache` finding is blocked with code `CORRUPT`. Keep each root under `SquireFixtures.scopedScratch` so cleanup is deterministic.

- [ ] **Step 6: Run the focused suite and record cache-root RED**

Expected: macOS and Windows cases report `OK` because doctor still inspects `home/.cache/morphir-scala`; Linux cases characterize the existing behavior.

- [ ] **Step 7: Implement cache-root parity**

Add helpers in `SquireDoctor`:

```scala
private def absoluteEnvironmentPath(platform: SquireEnv.Platform, name: String): Option[Path] =
  platform.environment.get(name).filter(_.nonEmpty).map(Path(_)).filter(_.toJava.isAbsolute)

private def defaultAcquisitionCacheRoot(platform: SquireEnv.Platform): Path =
  if platform.isMacOS then platform.home / "Library" / "Caches" / "morphir-scala"
  else if platform.isWindows then
    absoluteEnvironmentPath(platform, "LOCALAPPDATA")
      .getOrElse(platform.home / "AppData" / "Local") / "morphir-scala" / "Cache"
  else
    absoluteEnvironmentPath(platform, "XDG_CACHE_HOME")
      .getOrElse(platform.home / ".cache") / "morphir-scala"
```

Use the helper only when `MORPHIR_NODE_CACHE` is absent or empty. Leave explicit relative `MORPHIR_NODE_CACHE` handling unchanged so it remains a blocking `INVALID` finding.

- [ ] **Step 8: Add override precedence and safety assertions**

On a macOS test platform, create one corrupt cache under an absolute `MORPHIR_NODE_CACHE` override and a different clean default. Assert doctor reports the override corruption. Retain existing no-follow symlink, entry-count, per-entry-size, and total-hash-budget tests unchanged.

- [ ] **Step 9: Run GREEN and mutation-check the platform branches**

Run the focused suite. Temporarily swap the macOS and Windows branches, rerun, and confirm the corresponding filesystem-backed test fails; restore the production source exactly and rerun GREEN. Confirm `git diff --check` and that no temporary cache roots remain.

- [ ] **Step 10: Commit Task 2**

```bash
git add .claude/skills/squire/SquireEnv.scala .claude/skills/squire/SquireDoctor.scala .claude/skills/squire/SquireTests.scala
git commit -m "fix(squire): honor platform acquisition caches"
```

---

### Task 3: Supported-launcher and repository verification

**Files:**
- Modify only if a verification regression requires a scoped fix: `.claude/skills/squire/squire.scala`, `.claude/skills/squire/SquireEnv.scala`, `.claude/skills/squire/SquireDoctor.scala`, `.claude/skills/squire/SquireTests.scala`

**Interfaces:**
- Consumes: Task 1's positional resolution and Task 2's platform cache selection.
- Produces: final evidence for PR #956; no GitHub thread mutation.

- [ ] **Step 1: Exercise safe positional launcher forms**

Use scratch paths and read-only/missing manifest names so no remote or destructive operation can succeed:

```bash
.claude/skills/squire/squire reference repo status __squire_missing__
.claude/skills/squire/squire reference repo remove __squire_missing__
.claude/skills/squire/squire cellar deps __squire_unknown_alias__
```

Expected: commands pass Case App parsing, then return the typed domain error or operation result with no `Required option --... not specified`, JVM stack trace, or Mill `Subprocess failed` noise.

- [ ] **Step 2: Verify help and named alternatives**

Run help for all six affected commands and direct parser tests for the named forms. Expected: positionals appear in usage, named flags remain documented, and named forms leave no remaining arguments.

- [ ] **Step 3: Run formatting and the complete Squire gate**

```bash
mise run fmt
mise run test:squire
```

Expected: exit 0; all registered Kyo suites and snapshot/policy checks pass.

- [ ] **Step 4: Verify constraints and lint**

```bash
test "$(cat .claude/skills/squire/.mill-version)" = "1.2.0-RC1-24-042146"
! find .claude/skills/squire -type f \( -name '*.py' -o -name '*.ts' \) -print | grep .
git diff --check
mise run lint
```

Expected: exact pin, no Python/TypeScript files, clean diff, lint exit 0.

- [ ] **Step 5: Run full local CI sequentially**

```bash
mise run ci:local
```

Expected: exit 0 after Squire, JVM, JS/Wasm, Native, Morphir Elm, Mill plugin, runtime, and lint gates complete.

- [ ] **Step 6: Request a final code review**

Review the implementation range against the approved design. Require explicit confirmation that both GitHub findings are fixed, named flags remain supported, positional errors are fail-closed, platform predicates cannot disagree, cache safety limits remain intact, and no Critical/Important/Minor issues remain.

- [ ] **Step 7: Commit any review fix only after a new RED regression**

If review finds a valid defect, add a failing regression, apply the smallest fix, rerun Tasks 3.3 through 3.5, and create a focused human-authored commit. If review is clean, make no empty commit.

- [ ] **Step 8: Present integration status without mutating GitHub threads**

Report the commit SHAs, verification results, and the two unresolved thread IDs. Ask separately before replying to or resolving either GitHub thread. Push only under the user's existing explicit branch-publish authorization, preserve PR #956 as draft, and monitor all checks through completion.
