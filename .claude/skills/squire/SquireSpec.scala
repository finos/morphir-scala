//| scalaVersion: 3.8.4
//| moduleDeps: [SquireModel.scala, SquireProcess.scala, SquireSchemas.scala]

import java.nio.file.Files
import kyo.*
import scala.jdk.CollectionConverters.*

final case class SpecSyncOptions(
    ref: String = "main",
    dryRun: Boolean = false,
    theirs: Boolean = false,
    prune: Boolean = false,
    noFetch: Boolean = false,
    json: Boolean = false
) derives CanEqual

final case class SpecExportOptions(
    to: Maybe[Path] = Absent,
    dryRun: Boolean = false,
    includeDiverged: Boolean = false,
    branch: String = "morphir-kb/spec-sync",
    noBranch: Boolean = false,
    json: Boolean = false
) derives CanEqual

final case class SpecStep(
    step: String,
    status: String,
    detail: String,
    hint: Maybe[String] = Absent,
    result: Maybe[Structure.Value] = Absent
) derives Schema

final case class SpecReport(command: String, ok: Boolean, steps: List[SpecStep]) derives Schema

trait SquireSpecPlatform:
  def exists(path: Path): Boolean < Sync
  def isDirectory(path: Path): Boolean < Sync
  def isSymlink(path: Path): Boolean < Sync
  def resolve(path: Path): Path < Sync
  def findExecutable(name: String): Maybe[String]
  def glob(cwd: Path, pattern: String): Chunk[String] < Sync
  def compareSchemas(directory: Path): SchemaReport < (Sync & Abort[SquireError])

object LiveSquireSpecPlatform extends SquireSpecPlatform:
  def exists(path: Path): Boolean < Sync      = Sync.defer(Files.exists(path.toJava))
  def isDirectory(path: Path): Boolean < Sync = Sync.defer(Files.isDirectory(path.toJava))
  def isSymlink(path: Path): Boolean < Sync   = Sync.defer(Files.isSymbolicLink(path.toJava))
  def resolve(path: Path): Path < Sync        = Sync.defer(Path(path.toJava.toAbsolutePath.normalize.toString))

  def findExecutable(name: String): Maybe[String] =
    val requested = java.nio.file.Path.of(name)
    if requested.isAbsolute || name.contains(java.io.File.separator) then
      if Files.isExecutable(requested) then Present(requested.toString) else Absent
    else
      val extensions =
        if java.io.File.separatorChar == '\\' then
          Option(java.lang.System.getenv("PATHEXT"))
            .toList
            .flatMap(_.split(java.io.File.pathSeparator).toList)
            .map(_.toLowerCase)
            .prepended("")
        else List("")
      val directories = Option(java.lang.System.getenv("PATH")).toList
        .flatMap(_.split(java.io.File.pathSeparator).toList)
        .filter(_.nonEmpty)
      directories.iterator
        .flatMap(directory => extensions.iterator.map(extension => java.nio.file.Path.of(directory, name + extension)))
        .find(Files.isExecutable(_))
        .map(path => Present(path.toString))
        .getOrElse(Absent)

  def glob(cwd: Path, pattern: String): Chunk[String] < Sync =
    Sync.defer {
      if !Files.isDirectory(cwd.toJava) then Chunk.empty
      else
        val matcher = cwd.toJava.getFileSystem.getPathMatcher(s"glob:$pattern")
        val stream  = Files.walk(cwd.toJava)
        try
          Chunk.from(
            stream.iterator.asScala
              .filter(Files.isRegularFile(_))
              .map(path => cwd.toJava.relativize(path))
              .filter(matcher.matches)
              .map(_.toString.replace('\\', '/'))
              .toList
              .sorted
          )
        finally stream.close()
    }

  def compareSchemas(directory: Path): SchemaReport < (Sync & Abort[SquireError]) =
    SquireSchemas.compare(directory, directory, all = false)

object SquireSpec:
  val UpstreamRepo: String          = "finos/morphir"
  val UpstreamUrl: String           = "https://github.com/finos/morphir"
  val CheckoutRel: String           = ".refs/finos/morphir"
  val SparsePaths: Chunk[String]    = Chunk("docs", "website", "tests/bdd", "wit")
  val DefaultExportBranch: String   = "morphir-kb/spec-sync"
  private val KbRel: String         = ".claude/skills/kb/kb"
  private val SchemaRel: String     = "website/static/schemas"
  private val SchemaYamlGlob        = s"$SchemaRel/*.yaml"
  private val MorphirSchemaYamlGlob = s"$SchemaRel/morphir-ir-*.yaml"

  private final case class Validator(label: String, argv: Chunk[String], globbed: Boolean)

  private val Validators = List(
    Validator("jsonschema fmt", Chunk("jsonschema", "fmt", "--check", s"$SchemaRel/"), globbed = false),
    Validator("jsonschema lint", Chunk("jsonschema", "lint", SchemaYamlGlob), globbed = true),
    Validator("jsonschema metaschema", Chunk("jsonschema", "metaschema", SchemaYamlGlob), globbed = true)
  )

  def findRepoRoot(from: Path, platform: SquireSpecPlatform = LiveSquireSpecPlatform): Maybe[Path] < Sync =
    def loop(path: Path): Maybe[Path] < Sync =
      platform.exists(path / KbRel).flatMap { found =>
        if found then Present(path)
        else
          path.parent match
            case Present(parent) => loop(parent)
            case Absent          => Absent
      }
    platform.resolve(from).flatMap(loop)

  def sync(
      options: SpecSyncOptions,
      root: Path,
      runner: ProcessRunner
  ): SpecReport < (Async & Sync & Abort[SquireError]) =
    sync(options, root, runner, LiveSquireSpecPlatform)

  def sync(
      options: SpecSyncOptions,
      root: Path,
      runner: ProcessRunner,
      platform: SquireSpecPlatform
  ): SpecReport < (Async & Sync & Abort[SquireError]) =
    checkoutStep(root, options.prune, runner, platform, "spec-sync", Nil).flatMap {
      case Left(report)                      => report
      case Right((checkout, checkoutReport)) =>
        fetchStep(checkout, options, runner, checkoutReport.steps).flatMap {
          case Left(report)      => report
          case Right(fetchSteps) =>
            statusStep(root, runner, fetchSteps).flatMap {
              case Left(report)       => report
              case Right(statusSteps) =>
                pullStep(root, options, runner, statusSteps).flatMap {
                  case Left(report)     => report
                  case Right(pullSteps) => checkStep(root, options.json, runner, pullSteps)
                }
            }
        }
    }

  def `export`(
      options: SpecExportOptions,
      root: Path,
      runner: ProcessRunner
  ): SpecReport < (Async & Sync & Abort[SquireError]) =
    `export`(options, root, runner, LiveSquireSpecPlatform)

  def `export`(
      options: SpecExportOptions,
      root: Path,
      runner: ProcessRunner,
      platform: SquireSpecPlatform
  ): SpecReport < (Async & Sync & Abort[SquireError]) =
    val requested = options.to.getOrElse(root / CheckoutRel)
    platform.resolve(requested).flatMap { checkout =>
      platform.isDirectory(checkout).flatMap { valid =>
        if !valid then
          failed(
            "spec-export",
            Nil,
            "checkout",
            s"no checkout at $checkout",
            Present("run spec sync first, or pass --to with a path to a morphir checkout")
          )
        else
          exportCheckoutStep(checkout, runner).flatMap {
            case Left(report)         => report
            case Right(checkoutSteps) =>
              pushStep(root, checkout, options, runner, checkoutSteps).flatMap {
                case Left(report)                => statusExportStep(checkout, runner, report.steps, failures = 1)
                case Right((written, pushSteps)) =>
                  branchStep(checkout, options, runner, pushSteps).flatMap {
                    case Left(report)       => statusExportStep(checkout, runner, report.steps, failures = 1)
                    case Right(branchSteps) =>
                      validatorSteps(checkout, options.dryRun, written, runner, platform, branchSteps).flatMap {
                        case (validatorSteps, failures) => statusExportStep(checkout, runner, validatorSteps, failures)
                      }
                  }
              }
          }
      }
    }

  def renderText(report: SpecReport): String =
    val labels =
      if report.command == "spec-sync" then
        Map(
          "checkout" -> "[1/5] reference checkout",
          "fetch"    -> "[2/5] refresh finos/morphir",
          "status"   -> "[3/5] sync status",
          "pull"     -> "[4/5] sync pull",
          "check"    -> "[5/5] kb check"
        )
      else
        Map(
          "push"   -> "[1/4] sync push",
          "branch" -> "[2/4] branch",
          "status" -> "[4/4] checkout status"
        )
    val rows = report.steps.map { step =>
      val heading = labels.getOrElse(
        step.step,
        if step.step.startsWith("validator:") then s"[3/4] ${step.step.stripPrefix("validator:")}" else step.step
      )
      val marker = step.status match
        case "failed"       => "ERROR"
        case "skipped"      => "skipped"
        case "pre-existing" => "pre-existing"
        case _              => "ok"
      s"$heading\n  $marker: ${step.detail}"
    }
    val ending =
      if !report.ok then "Workflow failed."
      else if report.command == "spec-sync" then
        "Import complete. Review the diff, then edit in the knowledge base — not in .refs/."
      else
        "Nothing has been committed or pushed. Review the checkout, then commit there yourself."
    (rows :+ "" :+ ending).mkString("\n") + "\n"

  private def checkoutStep(
      root: Path,
      prune: Boolean,
      runner: ProcessRunner,
      platform: SquireSpecPlatform,
      command: String,
      steps: List[SpecStep]
  ): Either[SpecReport, (Path, SpecReport)] < (Async & Sync & Abort[SquireError]) =
    val checkout = root / CheckoutRel
    for
      gitExists <- platform.exists(checkout / ".git")
      symlink   <- platform.isSymlink(checkout)
      result    <-
        if !gitExists && !symlink then
          Sync.defer(
            Left(
              failedValue(
                command,
                steps,
                "checkout",
                s"no reference checkout of $UpstreamRepo at $CheckoutRel",
                Present(s"add one with:\n    ${addCommand}")
              )
            )
          )
        else
          runner.run(git(checkout, "rev-parse", "HEAD")).flatMap { headResult =>
            val head = headResult.stdout.trim
            if headResult.exitCode != 0 || head.isEmpty then
              Sync.defer(
                Left(
                  failedValue(
                    command,
                    steps,
                    "checkout",
                    s"$CheckoutRel is not a usable Git checkout with a HEAD",
                    optional(lastDetail(headResult))
                  )
                )
              )
            else
              runner.run(git(checkout, "config", "--get", "core.sparseCheckout")).flatMap { sparseResult =>
                sparseSetting(sparseResult) match
                  case Left(detail) =>
                    Sync.defer(
                      Left(
                        failedValue(
                          command,
                          steps,
                          "checkout",
                          "could not inspect sparse checkout configuration",
                          Present(detail)
                        )
                      )
                    )
                  case Right(sparse) =>
                    val missingEffect =
                      if sparse then missingSparsePaths(checkout, platform, SparsePaths.toList)
                      else Sync.defer(List.empty[String])
                    missingEffect.map { missing =>
                      if missing.nonEmpty && prune then
                        Left(
                          failedValue(
                            command,
                            steps,
                            "checkout",
                            s"sparse checkout is missing ${missing.mkString(", ")}, and --prune would delete the mirror's copy of everything under them",
                            Present(s"widen it: git -C $CheckoutRel sparse-checkout set ${SparsePaths.mkString(" ")}")
                          )
                        )
                      else
                        val detail =
                          if missing.nonEmpty then s"$checkout (missing ${missing.mkString(", ")})"
                          else checkout.toString
                        val result = record(
                          "commit"  -> Structure.Value.Str(head),
                          "sparse"  -> Structure.Value.Bool(sparse),
                          "missing" -> Structure.Value.Sequence(Chunk.from(missing.map(Structure.Value.Str(_))))
                        )
                        val next = SpecReport(
                          command,
                          ok = true,
                          steps :+ SpecStep("checkout", "ok", detail, result = Present(result))
                        )
                        Right(checkout -> next)
                    }
              }
          }
    yield result

  private def fetchStep(
      checkout: Path,
      options: SpecSyncOptions,
      runner: ProcessRunner,
      steps: List[SpecStep]
  ): Either[SpecReport, List[SpecStep]] < (Async & Abort[SquireError]) =
    if options.noFetch then Right(steps :+ SpecStep("fetch", "skipped", "--no-fetch"))
    else if options.dryRun then Right(steps :+ SpecStep("fetch", "skipped", "--dry-run"))
    else
      runner.run(git(checkout, "status", "--porcelain")).flatMap { dirty =>
        if dirty.exitCode != 0 then
          Left(
            failedValue(
              "spec-sync",
              steps,
              "fetch",
              s"could not inspect $CheckoutRel for uncommitted changes",
              optional(lastDetail(dirty))
            )
          )
        else if dirty.stdout.trim.nonEmpty then
          Left(
            failedValue(
              "spec-sync",
              steps,
              "fetch",
              s"$CheckoutRel has uncommitted changes",
              Present("commit, stash or discard them there, or re-run with --no-fetch")
            )
          )
        else
          runner.run(git(checkout, "fetch", "--depth", "1", "origin", options.ref)).flatMap { fetched =>
            if fetched.exitCode != 0 then
              Left(failedValue(
                "spec-sync",
                steps,
                "fetch",
                s"git fetch origin ${options.ref} failed: ${fetched.stderr.trim}"
              ))
            else
              runner.run(git(checkout, "checkout", "--detach", "FETCH_HEAD")).flatMap { detached =>
                if detached.exitCode != 0 then
                  Left(failedValue(
                    "spec-sync",
                    steps,
                    "fetch",
                    s"git checkout FETCH_HEAD failed: ${detached.stderr.trim}"
                  ))
                else
                  runner.run(git(checkout, "rev-parse", "HEAD")).map { head =>
                    if head.exitCode != 0 || head.stdout.trim.isEmpty then
                      Left(
                        failedValue(
                          "spec-sync",
                          steps,
                          "fetch",
                          "git checkout FETCH_HEAD did not leave a usable HEAD",
                          optional(lastDetail(head))
                        )
                      )
                    else
                      val result = record("commit" -> Structure.Value.Str(head.stdout.trim))
                      Right(
                        steps :+ SpecStep(
                          "fetch",
                          "ok",
                          s"$UpstreamRepo@${options.ref}",
                          result = Present(result)
                        )
                      )
                  }
              }
          }
      }

  private def statusStep(
      root: Path,
      runner: ProcessRunner,
      steps: List[SpecStep]
  ): Either[SpecReport, List[SpecStep]] < (Async & Abort[SquireError]) =
    runner.run(kb(root, "sync", "status", "--json")).map { process =>
      if process.exitCode != 0 then
        Left(
          failedValue(
            "spec-sync",
            steps,
            "status",
            s"kb sync status exited ${process.exitCode}",
            optional(lastDetail(process))
          )
        )
      else
        parseJson(process.stdout) match
          case None =>
            val detail = lastDetail(process)
            Left(failedValue("spec-sync", steps, "status", "kb sync status produced no JSON", optional(detail)))
          case Some(value) =>
            Right(steps :+ SpecStep("status", "ok", "kb sync status", result = Present(value)))
    }

  private def pullStep(
      root: Path,
      options: SpecSyncOptions,
      runner: ProcessRunner,
      steps: List[SpecStep]
  ): Either[SpecReport, List[SpecStep]] < (Async & Abort[SquireError]) =
    val arguments = Chunk("sync", "pull") ++
      (if options.dryRun then Chunk("--dry-run") else Chunk.empty) ++
      (if options.theirs then Chunk("--theirs") else Chunk.empty) ++
      (if options.prune then Chunk("--prune") else Chunk.empty) ++
      (if options.json then Chunk("--json") else Chunk.empty)
    runner.run(ProcessRequest(Chunk((root / KbRel).toString) ++ arguments, Present(root))).map { process =>
      val detail = arguments.mkString(" ")
      if process.exitCode != 0 then
        Left(
          failedValue(
            "spec-sync",
            steps,
            "pull",
            s"kb $detail exited ${process.exitCode}",
            optional(lastDetail(process))
          )
        )
      else
        val result = if options.json then parseJson(process.stdout).map(Present(_)).getOrElse(Absent) else Absent
        Right(steps :+ SpecStep("pull", "ok", detail, result = result))
    }

  private def checkStep(
      root: Path,
      json: Boolean,
      runner: ProcessRunner,
      steps: List[SpecStep]
  ): SpecReport < (Async & Abort[SquireError]) =
    val arguments = Chunk("check", "--no-provenance") ++ (if json then Chunk("--json") else Chunk.empty)
    runner.run(ProcessRequest(Chunk((root / KbRel).toString) ++ arguments, Present(root))).map { process =>
      val status = if process.exitCode == 0 then "ok" else "failed"
      val result = if json then parseJson(process.stdout).map(Present(_)).getOrElse(Absent) else Absent
      val baseDetail = arguments.mkString(" ")
      val diagnostics = if process.exitCode != 0 && !json then processDiagnostics(process) else ""
      val detail = if diagnostics.nonEmpty then s"$baseDetail\n$diagnostics" else baseDetail
      SpecReport(
        "spec-sync",
        ok = process.exitCode == 0,
        steps :+ SpecStep("check", status, detail, result = result)
      )
    }

  private def pushStep(
      root: Path,
      checkout: Path,
      options: SpecExportOptions,
      runner: ProcessRunner,
      steps: List[SpecStep]
  ): Either[SpecReport, (List[String], List[SpecStep])] < Async =
    val displayArguments = Chunk("sync", "push", "--to", checkout.toString) ++
      (if options.dryRun then Chunk("--dry-run") else Chunk.empty) ++
      (if options.includeDiverged then Chunk("--include-diverged") else Chunk.empty) ++
      (if options.json then Chunk("--json") else Chunk.empty)
    val processArguments =
      if displayArguments.contains("--json") then displayArguments else displayArguments ++ Chunk("--json")
    val request = ProcessRequest(Chunk((root / KbRel).toString) ++ processArguments, Present(root))
    Abort.run[SquireError](runner.run(request)).map {
      case Result.Failure(error) =>
        Left(
          SpecReport(
            "spec-export",
            ok = false,
            steps :+ SpecStep("push", "failed", s"kb ${displayArguments.mkString(" ")}: ${error.getMessage}")
          )
        )
      case Result.Success(process) =>
        val display = displayArguments.mkString(" ")
        val payload = parseJson(process.stdout)
        if process.exitCode != 0 then
          Left(
            SpecReport(
              "spec-export",
              ok = false,
              steps :+ SpecStep(
                "push",
                "failed",
                s"kb $display exited ${process.exitCode}",
                optional(lastDetail(process)),
                payload.map(Present(_)).getOrElse(Absent)
              )
            )
          )
        else
          payload match
            case None =>
              Left(
                failedValue(
                  "spec-export",
                  steps,
                  "push",
                  s"kb $display produced no valid JSON ownership report",
                  optional(lastDetail(process))
                )
              )
            case Some(value) =>
              writtenPaths(value) match
                case Left(detail) =>
                  Left(
                    SpecReport(
                      "spec-export",
                      ok = false,
                      steps :+ SpecStep(
                        "push",
                        "failed",
                        s"kb $display produced invalid ownership JSON: $detail",
                        result = Present(value)
                      )
                    )
                  )
                case Right(written) =>
                  Right(
                    written ->
                      (steps :+ SpecStep(
                        "push",
                        "ok",
                        s"$display (${written.size} written path(s))",
                        result = Present(value)
                      ))
                  )
    }

  private def exportCheckoutStep(
      checkout: Path,
      runner: ProcessRunner
  ): Either[SpecReport, List[SpecStep]] < Async =
    Abort.run[SquireError](runner.run(git(checkout, "rev-parse", "--is-inside-work-tree"))).map {
      case Result.Failure(error) =>
        Left(failedValue("spec-export", Nil, "checkout", s"could not inspect target checkout: ${error.getMessage}"))
      case Result.Success(process) if process.exitCode == 0 && process.stdout.trim == "true" => Right(Nil)
      case Result.Success(process)                                                           =>
        Left(
          failedValue(
            "spec-export",
            Nil,
            "checkout",
            s"$checkout is not a working Git checkout",
            optional(lastDetail(process))
          )
        )
    }

  private def branchStep(
      checkout: Path,
      options: SpecExportOptions,
      runner: ProcessRunner,
      steps: List[SpecStep]
  ): Either[SpecReport, List[SpecStep]] < Async =
    if options.noBranch then Right(steps :+ SpecStep("branch", "skipped", "--no-branch"))
    else if options.dryRun then Right(steps :+ SpecStep("branch", "skipped", "--dry-run"))
    else
      Abort.run[SquireError](runner.run(git(checkout, "switch", "-c", options.branch))).flatMap {
        case Result.Failure(error) =>
          Left(failedValue("spec-export", steps, "branch", s"cannot create ${options.branch}: ${error.getMessage}"))
        case Result.Success(created) if created.exitCode == 0 =>
          Right(steps :+ SpecStep(
            "branch",
            "ok",
            s"${options.branch} created",
            result = Present(record("created" -> Structure.Value.Bool(true)))
          ))
        case Result.Success(created) =>
          Abort.run[SquireError](runner.run(git(checkout, "switch", options.branch))).map {
            case Result.Failure(error) =>
              Left(
                failedValue(
                  "spec-export",
                  steps,
                  "branch",
                  s"cannot switch to ${options.branch}: ${error.getMessage}",
                  Present("the checkout may have uncommitted changes on another branch")
                )
              )
            case Result.Success(existing) if existing.exitCode == 0 =>
              Right(
                steps :+ SpecStep(
                  "branch",
                  "ok",
                  s"${options.branch} already existed — switched to it",
                  result = Present(record("created" -> Structure.Value.Bool(false)))
                )
              )
            case Result.Success(_) =>
              Left(
                failedValue(
                  "spec-export",
                  steps,
                  "branch",
                  s"cannot switch to ${options.branch}: ${created.stderr.trim}",
                  Present("the checkout may have uncommitted changes on another branch")
                )
              )
          }
      }

  private def validatorSteps(
      checkout: Path,
      dryRun: Boolean,
      written: List[String],
      runner: ProcessRunner,
      platform: SquireSpecPlatform,
      steps: List[SpecStep]
  ): (List[SpecStep], Int) < (Async & Sync) =
    externalValidators(checkout, dryRun, written, runner, platform, Validators, steps, 0).flatMap {
      case (externalSteps, failures) =>
        schemaValidator(checkout, dryRun, written, platform, externalSteps, failures)
    }

  private def externalValidators(
      checkout: Path,
      dryRun: Boolean,
      written: List[String],
      runner: ProcessRunner,
      platform: SquireSpecPlatform,
      remaining: List[Validator],
      steps: List[SpecStep],
      failures: Int
  ): (List[SpecStep], Int) < (Async & Sync) =
    remaining match
      case Nil               => (steps, failures)
      case validator :: tail =>
        externalValidator(checkout, dryRun, written, runner, platform, validator).flatMap { step =>
          externalValidators(
            checkout,
            dryRun,
            written,
            runner,
            platform,
            tail,
            steps :+ step,
            failures + (if step.status == "failed" then 1 else 0)
          )
        }

  private def externalValidator(
      checkout: Path,
      dryRun: Boolean,
      written: List[String],
      runner: ProcessRunner,
      platform: SquireSpecPlatform,
      validator: Validator
  ): SpecStep < (Async & Sync) =
    val name = s"validator:${validator.label}"
    platform.findExecutable("jsonschema") match
      case Absent              => SpecStep(name, "skipped", "jsonschema not on PATH")
      case Present(executable) =>
        platform.isDirectory(checkout / SchemaRel).flatMap { present =>
          if !present then SpecStep(name, "skipped", s"$SchemaRel absent (sparse checkout? npm install?)")
          else
            expand(checkout, validator.argv.updated(0, executable), platform).flatMap {
              case None                 => SpecStep(name, "skipped", "no matching files")
              case Some(argv) if dryRun => SpecStep(name, "skipped", "--dry-run")
              case Some(argv)           =>
                Abort.run[SquireError](runner.run(ProcessRequest(argv, Present(checkout)))).map {
                  case Result.Failure(error)   => SpecStep(name, "failed", error.getMessage)
                  case Result.Success(process) =>
                    val output = (process.stdout + process.stderr).trim
                    if process.exitCode == 0 then SpecStep(name, "ok", validator.argv.mkString(" "))
                    else if output.contains("does not support YAML") then
                      SpecStep(name, "skipped", "tool does not support YAML input", result = optionalValue(output))
                    else
                      val ours   = ownsSchemas(written)
                      val status = if ours then "failed" else "pre-existing"
                      SpecStep(name, status, validator.argv.mkString(" "), result = optionalValue(output))
                }
            }
        }

  private def schemaValidator(
      checkout: Path,
      dryRun: Boolean,
      written: List[String],
      platform: SquireSpecPlatform,
      steps: List[SpecStep],
      failures: Int
  ): (List[SpecStep], Int) < Sync =
    val name      = "validator:schemas json in step"
    val directory = checkout / SchemaRel
    platform.isDirectory(directory).flatMap { present =>
      if !present then
        (steps :+ SpecStep(name, "skipped", s"$SchemaRel absent (sparse checkout? npm install?)"), failures)
      else
        platform.glob(checkout, MorphirSchemaYamlGlob).flatMap { matches =>
          if matches.isEmpty then (steps :+ SpecStep(name, "skipped", "no matching files"), failures)
          else if dryRun then (steps :+ SpecStep(name, "skipped", "--dry-run"), failures)
          else
            Abort.run[SquireError](platform.compareSchemas(directory)).map {
              case Result.Success(report) =>
                val status = if report.ok then "ok" else if ownsSchemas(written) then "failed" else "pre-existing"
                val value  = parseJson(SquireJson.encode(report)).map(Present(_)).getOrElse(Absent)
                val step   = SpecStep(name, status, "Scala SquireSchemas.compare", result = value)
                (steps :+ step, failures + (if status == "failed" then 1 else 0))
              case Result.Failure(error) =>
                val status = if ownsSchemas(written) then "failed" else "pre-existing"
                val step   = SpecStep(
                  name,
                  status,
                  s"Scala SquireSchemas.compare: ${error.getMessage}",
                  result = Present(Structure.Value.Str(error.getMessage))
                )
                (steps :+ step, failures + (if status == "failed" then 1 else 0))
            }
        }
    }

  private def statusExportStep(
      checkout: Path,
      runner: ProcessRunner,
      steps: List[SpecStep],
      failures: Int
  ): SpecReport < Async =
    Abort.run[SquireError](runner.run(git(checkout, "status", "--short"))).map {
      case Result.Failure(error) =>
        SpecReport(
          "spec-export",
          ok = false,
          steps :+ SpecStep("status", "failed", s"could not inspect checkout status: ${error.getMessage}")
        )
      case Result.Success(process) if process.exitCode != 0 =>
        SpecReport(
          "spec-export",
          ok = false,
          steps :+ SpecStep(
            "status",
            "failed",
            s"git status --short exited ${process.exitCode}",
            optional(lastDetail(process))
          )
        )
      case Result.Success(process) =>
        val changed = process.stdout.linesIterator.filter(_.nonEmpty).toList
        val result  = record("changed" -> Structure.Value.Sequence(Chunk.from(changed.map(Structure.Value.Str(_)))))
        SpecReport(
          "spec-export",
          ok = failures == 0,
          steps :+ SpecStep("status", "ok", s"${changed.size} changed path(s)", result = Present(result))
        )
    }

  private def expand(
      cwd: Path,
      argv: Chunk[String],
      platform: SquireSpecPlatform
  ): Option[Chunk[String]] < Sync =
    def loop(remaining: List[String], accumulated: Chunk[String]): Option[Chunk[String]] < Sync =
      remaining match
        case Nil                                        => Some(accumulated)
        case argument :: tail if argument.contains('*') =>
          platform.glob(cwd, argument).flatMap { matches =>
            if matches.isEmpty then None else loop(tail, accumulated ++ matches)
          }
        case argument :: tail => loop(tail, accumulated :+ argument)
    loop(argv.toList, Chunk.empty)

  private def missingSparsePaths(
      checkout: Path,
      platform: SquireSpecPlatform,
      remaining: List[String]
  ): List[String] < Sync =
    remaining match
      case Nil          => Nil
      case path :: tail =>
        platform.isDirectory(checkout / path).flatMap { exists =>
          missingSparsePaths(checkout, platform, tail).map(rest => if exists then rest else path :: rest)
        }

  private def parseJson(text: String): Option[Structure.Value] =
    val trimmed                                        = text.trim
    def decode(value: String): Option[Structure.Value] =
      Json.decode[Structure.Value](value) match
        case Result.Success(parsed) => Some(parsed)
        case _                      => None
    if trimmed.isEmpty then None
    else
      decode(trimmed).orElse {
        val objectStart = text.indexOf('{')
        val arrayStart  = text.indexOf('[')
        List(objectStart, arrayStart).filter(_ >= 0).sorted.iterator
          .flatMap(start => decode(text.substring(start).trim))
          .nextOption()
      }

  private def writtenPaths(value: Structure.Value): Either[String, List[String]] =
    value match
      case Structure.Value.Record(fields) =>
        fields.filter(_._1 == "actions").toList match
          case Nil                                          => Left("missing actions array")
          case List((_, Structure.Value.Sequence(actions))) =>
            actions.toList.zipWithIndex.foldLeft[Either[String, List[String]]](Right(Nil)) {
              case (left @ Left(_), _)                                       => left
              case (Right(written), (Structure.Value.Record(action), index)) =>
                val verbs = action.filter(_._1 == "verb").toList
                val paths = action.filter(_._1 == "path").toList
                (verbs, paths) match
                  case (List((_, Structure.Value.Str(verb))), List((_, Structure.Value.Str(path)))) if path.nonEmpty =>
                    Right(if verb == "wrote" then written :+ path else written)
                  case (List((_, Structure.Value.Str(_))), _) =>
                    Left(s"action $index requires exactly one non-empty string path")
                  case _ => Left(s"action $index requires exactly one string verb")
              case (Right(_), (_, index)) => Left(s"action $index must be an object")
            }
          case List(_) => Left("actions must be an array")
          case _       => Left("ownership report must contain exactly one actions array")
      case _ => Left("ownership report must be an object")

  private def sparseSetting(result: ProcessResult): Either[String, Boolean] =
    if result.exitCode == 0 then
      result.stdout.trim.toLowerCase match
        case "true" | "yes" | "on" | "1"  => Right(true)
        case "false" | "no" | "off" | "0" => Right(false)
        case value => Left(s"invalid core.sparseCheckout value: ${if value.isEmpty then "<empty>" else value}")
    else if result.exitCode == 1 && result.stdout.trim.isEmpty && result.stderr.trim.isEmpty then Right(false)
    else Left(lastDetail(result))

  private def ownsSchemas(written: List[String]): Boolean = written.exists(_.startsWith(SchemaRel))

  private def addCommand: String =
    s"squire reference repo add $UpstreamUrl --sparse ${SparsePaths.mkString(" ")}"

  private def git(checkout: Path, args: String*): ProcessRequest =
    ProcessRequest(Chunk("git", "-C", checkout.toString) ++ Chunk.from(args))

  private def kb(root: Path, args: String*): ProcessRequest =
    ProcessRequest(Chunk((root / KbRel).toString) ++ Chunk.from(args), Present(root))

  private def failed(
      command: String,
      steps: List[SpecStep],
      name: String,
      detail: String,
      hint: Maybe[String] = Absent
  ): SpecReport = failedValue(command, steps, name, detail, hint)

  private def failedValue(
      command: String,
      steps: List[SpecStep],
      name: String,
      detail: String,
      hint: Maybe[String] = Absent
  ): SpecReport = SpecReport(command, ok = false, steps :+ SpecStep(name, "failed", detail, hint))

  private def record(fields: (String, Structure.Value)*): Structure.Value =
    Structure.Value.Record(Chunk.from(fields))

  private def optional(value: String): Maybe[String]               = if value.nonEmpty then Present(value) else Absent
  private def optionalValue(value: String): Maybe[Structure.Value] =
    if value.nonEmpty then Present(Structure.Value.Str(value)) else Absent

  private def lastDetail(result: ProcessResult): String =
    val combined = if result.stderr.trim.nonEmpty then result.stderr.trim else result.stdout.trim
    combined.linesIterator.toList.lastOption.getOrElse("no output")

  private def processDiagnostics(result: ProcessResult): String =
    List(result.stdout.trim, result.stderr.trim).filter(_.nonEmpty).mkString("\n")
