//| scalaVersion: 3.8.4
//| mainClass: SquireApp
//| moduleDeps: [SquireModel.scala, SquireProcess.scala, SquireEnv.scala, SquireDoctor.scala, SquireCellar.scala, SquireRepo.scala, SquireBranch.scala, SquireTracking.scala, SquireSchemas.scala, SquireSpec.scala]
//| mvnDeps:
//| - io.getkyo::kyo-case-app:1.0.0-RC6

import caseapp.*
import caseapp.core.app.CommandsEntryPoint
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, StandardOpenOption}
import kyo.*

final case class AiEnvInfoOpts(
    @HelpMessage("Run one named check and return its status") check: Option[String] = None,
    @HelpMessage("Bound live probes in seconds") timeout: Double = 8.0
)

final case class DoctorOpts()

@ArgsName("<coordinate> <symbol>")
final case class CellarGetOpts(
    @HelpMessage("Maven coordinate or project alias") coordinate: Option[String] = None,
    @HelpMessage("Fully qualified symbol") symbol: Option[String] = None,
    @HelpMessage("Hide inherited members") hideInherited: Boolean = false,
    @HelpMessage("Group inherited members") groupInherited: Boolean = false,
    @HelpMessage("Maximum number of results") limit: Option[Int] = None,
    @HelpMessage("Absolute writable temp directory for Cellar") tempDirectory: Option[String] = None
)

@ArgsName("<coordinate> <query>")
final case class CellarSearchOpts(
    @HelpMessage("Maven coordinate or project alias") coordinate: Option[String] = None,
    @HelpMessage("Symbol-name substring") query: Option[String] = None,
    @HelpMessage("Maximum number of results") limit: Option[Int] = None,
    @HelpMessage("Absolute writable temp directory for Cellar") tempDirectory: Option[String] = None
)

@ArgsName("<coordinate>")
final case class CellarDepsOpts(
    @HelpMessage("Maven coordinate or project alias") coordinate: Option[String] = None,
    @HelpMessage("Absolute writable temp directory for Cellar") tempDirectory: Option[String] = None
)

@ArgsName("<url-or-path>")
final case class ReferenceRepoAddOpts(
    @HelpMessage("Git URL or local repository path") urlOrPath: Option[String] = None,
    @HelpMessage("Override the repository name") name: Option[String] = None,
    @HelpMessage("Branch, tag, or commit to check out") ref: Option[String] = None,
    @HelpMessage("Reference repository strategy") strategy: String = "clone",
    @HelpMessage("Shallow clone depth") depth: Option[Int] = None,
    @HelpMessage("Clone complete history") full: Boolean = false,
    @HelpMessage("Subtrees to materialise") sparse: List[String] = Nil
)

final case class ReferenceRepoListOpts(@HelpMessage("Output the raw manifest as JSON") json: Boolean = false)
@ArgsName("<name>")
final case class ReferenceRepoStatusOpts(@HelpMessage("Repository name") name: Option[String] = None)
@ArgsName("<name>")
final case class ReferenceRepoRemoveOpts(
    @HelpMessage("Repository name") name: Option[String] = None,
    @HelpMessage("Leave checkout files in place") keepFiles: Boolean = false
)

final case class BranchRefreshOpts(
    @HelpMessage("Target branch to refresh") target: String = "develop",
    @HelpMessage("Report the update without writing") dryRun: Boolean = false,
    @HelpMessage("Output the typed result as JSON") json: Boolean = false
)

final case class TrackingStatusOpts(
    @HelpMessage("Print only the effective tracking mode") quiet: Boolean = false,
    @HelpMessage("Exit successfully only when the effective mode matches") check: Option[String] = None
)
final case class TrackingSyncOpts(
    @HelpMessage("Report guidance drift without writing") check: Boolean = false,
    @HelpMessage("Print the pending guidance diff without writing") diff: Boolean = false
)
final case class TrackingDoctorOpts()

final case class SpecSyncOpts(
    @HelpMessage("Report changes without writing") dryRun: Boolean = false,
    @HelpMessage("Upstream ref to import") ref: Option[String] = None,
    @HelpMessage("Take the upstream side of conflicts") theirs: Boolean = false,
    @HelpMessage("Remove files deleted upstream") prune: Boolean = false,
    @HelpMessage("Use the checkout without fetching") noFetch: Boolean = false,
    @HelpMessage("Output the typed result as JSON") json: Boolean = false
)

final case class SpecExportOpts(
    @HelpMessage("Target Morphir checkout") to: Option[String] = None,
    @HelpMessage("Report changes without writing") dryRun: Boolean = false,
    @HelpMessage("Include changes that also moved upstream") includeDiverged: Boolean = false,
    @HelpMessage("Branch for exported changes") branch: String = "morphir-kb/spec-sync",
    @HelpMessage("Do not create or switch a branch") noBranch: Boolean = false,
    @HelpMessage("Output the typed result as JSON") json: Boolean = false
)

final case class SchemasBuildOpts(
    @HelpMessage("Input schema directory") from: Option[String] = None,
    @HelpMessage("Output schema directory") out: Option[String] = None,
    @HelpMessage("Include every morphir-*.yaml schema") all: Boolean = false,
    @HelpMessage("Output the typed result as JSON") json: Boolean = false
)

final case class SchemasCompareOpts(
    @HelpMessage("Input schema directory") from: Option[String] = None,
    @HelpMessage("Output schema directory") out: Option[String] = None,
    @HelpMessage("Include every morphir-*.yaml schema") all: Boolean = false,
    @HelpMessage("Output the typed result as JSON") json: Boolean = false
)

final case class SchemasValidateOpts(
    @HelpMessage("Schema directory") schemas: Option[String] = None,
    @HelpMessage("Document directory") documents: Option[String] = None,
    @HelpMessage("Output the typed result as JSON") json: Boolean = false
)

object SquireCli:
  private val ExitFileEnvironment = "SQUIRE_EXIT_FILE"

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
      val resolved = fields.map { case (name, named) => name -> named.orElse(remaining.dequeueFirst(_ => true)) }
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
      val extra = positional.drop(consumed)
      if extra.nonEmpty then cliFailure(command, s"unexpected positional arguments: ${extra.mkString(" ")}")
      else resolved

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

  def runCommand[S](
      operation: Int < (S & Abort[SquireError]),
      errorOutput: String => Unit = java.lang.System.err.print,
      recordExit: Int => Unit = recordExitCode
  ): Unit < (S & Sync) =
    Abort.run[SquireError](operation).flatMap {
      case Result.Success(exitCode) => Sync.defer(recordExit(validatedExit(exitCode, errorOutput)))
      case Result.Failure(error)    => Sync.defer {
          errorOutput(renderError(error))
          recordExit(1)
        }
    }

  private def validatedExit(exitCode: Int, errorOutput: String => Unit): Int =
    if exitCode >= 0 && exitCode <= 255 then exitCode
    else
      errorOutput(s"ERROR [cli]: command returned invalid exit code $exitCode; expected 0 through 255\n")
      1

  private def renderError(error: SquireError): String =
    error match
      case SquireError.Failure(area, message, detail) =>
        s"ERROR [$area]: $message\n" + detail.map(value => s"  $value\n").getOrElse("")

  private def recordExitCode(exitCode: Int): Unit =
    Option(java.lang.System.getenv(ExitFileEnvironment)).filter(_.nonEmpty).foreach { value =>
      val path = java.nio.file.Path.of(value)
      if !Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(path) then
        throw java.io.IOException(s"Squire exit handoff is not a regular file: $path")
      Files.writeString(
        path,
        s"$exitCode\n",
        StandardCharsets.UTF_8,
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.WRITE,
        LinkOption.NOFOLLOW_LINKS
      )
    }

  def projectRoot(from: Path): Path < Sync =
    SquirePaths.findRepoRoot(from).map(_.getOrElse(from))

  private def timeoutDuration(seconds: Double): Duration =
    math.max(0.0, seconds).*(1000000000L).toLong.nanos

  def runEnvInfo(
      options: AiEnvInfoOpts,
      root: Path,
      platform: SquireEnv.Platform,
      output: String => Unit
  ): Int < Sync =
    options.check match
      case None => SquireEnv.report(timeoutDuration(options.timeout), platform, root).map { report =>
          output(SquireEnv.renderLegacyReport(report))
          0
        }
      case Some("jvm-network") =>
        SquireEnv.check(SquireEnv.CheckKind.JvmNetwork, timeoutDuration(options.timeout), platform).map(if _ then 0
        else 1)
      case Some("var-folders") =>
        SquireEnv.check(SquireEnv.CheckKind.VarFolders, timeoutDuration(options.timeout), platform).map(if _ then 0
        else 1)
      case Some(_) => 2

  def printDoctor(report: SquireDoctor.DoctorReport): Int < Sync =
    Sync.defer {
      report.findings.foreach { finding =>
        val prefix = if finding.blocked then "ISSUE - " else ""
        java.lang.System.out.println(s"$prefix${finding.code} - ${finding.message}")
      }
      if report.findings.exists(_.blocked) then 1 else 0
    }

  def runCellar(
      action: CellarAction,
      root: Path,
      runner: ProcessRunner,
      platform: SquirePlatform,
      output: String => Unit,
      errorOutput: String => Unit,
      tempDirectory: Option[String] = None
  ): Int < (Async & Abort[SquireError]) =
    SquireCellar.run(action, root, runner, platform, tempDirectory).map { result =>
      if result.stdout.nonEmpty then output(result.stdout)
      if result.stderr.nonEmpty then errorOutput(result.stderr)
      result.exitCode
    }

  def runReferenceAdd(
      urlOrPath: String,
      options: ReferenceRepoAddOpts,
      root: Path,
      runner: ProcessRunner,
      platform: SquirePlatform,
      output: String => Unit
  ): Int < (Async & Sync & Abort[SquireError]) =
    SquireRepo
      .add(
        ReferenceAdd(
          urlOrPath,
          options.name,
          options.ref,
          options.strategy,
          options.depth,
          options.full,
          options.sparse
        ),
        root,
        runner,
        platform
      )
      .map { entry =>
        output(s"Added '${entry.name}' to .refs/ manifest.\n")
        output(SquireJson.encode(entry) + "\n")
        0
      }

  def runReferenceList(
      options: ReferenceRepoListOpts,
      root: Path,
      runner: ProcessRunner,
      output: String => Unit
  ): Int < (Async & Sync & Abort[SquireError]) =
    SquireRepo.list(root, options.json, runner).map { rendered =>
      output(rendered)
      0
    }

  def runReferenceStatus(
      name: Option[String],
      root: Path,
      runner: ProcessRunner,
      output: String => Unit
  ): Int < (Async & Sync & Abort[SquireError]) =
    SquireRepo.status(root, name, runner).map { report =>
      output(report.output)
      report.exitCode
    }

  def runReferenceRemove(
      name: String,
      keepFiles: Boolean,
      root: Path,
      runner: ProcessRunner,
      platform: SquirePlatform,
      output: String => Unit
  ): Int < (Async & Sync & Abort[SquireError]) =
    SquireRepo.remove(name, keepFiles, root, runner, platform).map { _ =>
      output(s"Removed '$name' from manifest.\n")
      0
    }

  def runBranch(
      options: BranchRefreshOpts,
      runner: ProcessRunner,
      output: String => Unit
  ): Int < (Async & Abort[SquireError]) =
    runBranch(options, Seq.empty, runner, output)

  def runBranch(
      options: BranchRefreshOpts,
      positional: Seq[String],
      runner: ProcessRunner,
      output: String => Unit
  ): Int < (Async & Abort[SquireError]) =
    if positional.nonEmpty then
      Abort.fail(
        SquireError.Failure(
          "cli",
          s"unexpected positional arguments for branch refresh: ${positional.mkString(" ")}"
        )
      )
    else
      SquireBranch.refresh(options.target, options.dryRun, runner).map { result =>
        if options.json then output(SquireJson.encode(result) + "\n")
        else output(s"${result.kind}: ${result.target} ${result.oldSha} -> ${result.newSha}\n")
        0
      }

  def runTrackingStatus(
      options: TrackingStatusOpts,
      root: Path,
      runner: ProcessRunner,
      platform: SquirePlatform,
      output: String => Unit
  ): Int < (Async & Sync & Abort[SquireError]) =
    options.check match
      case Some(expected) if !Set("auto", "beads", "off", "unavailable").contains(expected.toLowerCase) => 2
      case _ => SquireTracking.resolve(root, runner, platform).map { report =>
          options.check match
            case Some(expected)        => if report.effectiveMode.toString.equalsIgnoreCase(expected) then 0 else 1
            case None if options.quiet =>
              output(report.effectiveMode.toString.toLowerCase + "\n")
              0
            case None =>
              output(renderTrackingReport(report))
              0
        }

  private def renderTrackingReport(report: TrackingReport): String =
    import Structure.Value

    def mode(value: TrackingMode): String =
      value match
        case TrackingMode.Auto        => "auto"
        case TrackingMode.Beads       => "beads"
        case TrackingMode.Off         => "off"
        case TrackingMode.Unavailable => "unavailable"

    def optionalString(name: String, value: Maybe[String]): Chunk[(String, Value)] =
      value match
        case Present(text) => Chunk(name -> Value.Str(text))
        case Absent        => Chunk.empty

    val workspace       = report.workspace
    val workspaceFields =
      if workspace.status == "no-repo" then
        Chunk("status" -> Value.Str(workspace.status)) ++
          optionalString("detail", workspace.detail) ++
          optionalString("remedy", workspace.remedy)
      else
        Chunk(
          "is_worktree" -> Value.Bool(workspace.isWorktree),
          "local_store" -> Value.Bool(workspace.localStore)
        ) ++
          optionalString("fallback_config", workspace.fallbackConfig) ++
          Chunk("status" -> Value.Str(workspace.status)) ++
          optionalString("detail", workspace.detail) ++
          optionalString("remedy", workspace.remedy)

    val fields = Chunk(
      "configured_mode" -> Value.Str(mode(report.configuredMode)),
      "effective_mode"  -> Value.Str(mode(report.effectiveMode)),
      "reason"          -> Value.Str(report.reason),
      "bd"              -> Value.Record(
        Chunk(
          "installed" -> Value.Bool(report.bdInstalled),
          "version"   -> report.bdVersion.map(Value.Str(_)).getOrElse(Value.Null)
        )
      ),
      "beads_dir_present"     -> Value.Bool(report.beadsDirPresent),
      "workspace"             -> Value.Record(workspaceFields),
      "settings_file"         -> Value.Str(SquireTracking.settingsFile),
      "settings_file_present" -> Value.Bool(report.settingsFilePresent),
      "guidance_doc"          -> Value.Str(SquireTracking.guidanceDoc),
      "guidance_drift"        -> Value.Sequence(
        report.guidanceDrift.map(drift =>
          Value.Record(Chunk("file" -> Value.Str(drift.file), "issue" -> Value.Str(drift.issue)))
        )
      )
    ) ++ optionalString("warning", report.warning)

    SquireJson.pretty(Value.Record(fields))

  def runTrackingSync(
      options: TrackingSyncOpts,
      root: Path,
      output: String => Unit
  ): Int < Sync =
    if options.check && options.diff then 2
    else
      val mode = if options.diff then SquireTracking.GuidanceMode.Diff
      else if options.check then SquireTracking.GuidanceMode.Check else SquireTracking.GuidanceMode.Apply
      SquireTracking.syncGuidance(root, mode).map { report =>
        output(report.output)
        report.exitCode
      }

  def runTrackingDoctor(
      root: Path,
      runner: ProcessRunner,
      platform: SquirePlatform,
      output: String => Unit
  ): Int < (Async & Sync & Abort[SquireError]) =
    for
      report   <- SquireTracking.resolve(root, runner, platform)
      guidance <- SquireTracking.syncGuidance(root, SquireTracking.GuidanceMode.Check)
    yield
      output(renderTrackingReport(report))
      output("guidance:\n")
      output(guidance.output)
      if guidance.exitCode == 0 then 0 else 1

  def runSpecSync(
      options: SpecSyncOpts,
      root: Path,
      runner: ProcessRunner,
      platform: SquireSpecPlatform,
      output: String => Unit,
      errorOutput: String => Unit
  ): Int < (Async & Sync & Abort[SquireError]) =
    SquireSpec
      .sync(
        SpecSyncOptions(
          ref = options.ref.getOrElse("main"),
          dryRun = options.dryRun,
          theirs = options.theirs,
          prune = options.prune,
          noFetch = options.noFetch,
          json = options.json
        ),
        root,
        runner,
        platform
      )
      .map(report => emitSpecReport(report, options.json, output, errorOutput))

  def runSpecExport(
      options: SpecExportOpts,
      root: Path,
      runner: ProcessRunner,
      platform: SquireSpecPlatform,
      output: String => Unit,
      errorOutput: String => Unit
  ): Int < (Async & Sync & Abort[SquireError]) =
    val target = options.to.map { value =>
      val path = Path(value)
      if path.toJava.isAbsolute then path else root / value
    }
    SquireSpec
      .`export`(
        SpecExportOptions(
          to = Maybe.fromOption(target),
          dryRun = options.dryRun,
          includeDiverged = options.includeDiverged,
          branch = options.branch,
          noBranch = options.noBranch,
          json = options.json
        ),
        root,
        runner,
        platform
      )
      .map(report => emitSpecReport(report, options.json, output, errorOutput))

  private def emitSpecReport(
      report: SpecReport,
      json: Boolean,
      output: String => Unit,
      errorOutput: String => Unit
  ): Int =
    output(if json then SquireJson.encode(report) + "\n" else SquireSpec.renderText(report))
    if !report.ok then
      report.steps.find(_.status == "failed").orElse(report.steps.lastOption).foreach { step =>
        errorOutput(s"ERROR: ${step.detail}\n")
      }
    if report.ok then 0 else 1

  def runSchemasBuild(
      options: SchemasBuildOpts,
      root: Path,
      output: String => Unit,
      errorOutput: String => Unit
  ): Int < (Sync & Abort[SquireError]) =
    val from = resolveOption(root, options.from, SquireSchemas.DefaultFrom)
    val to   = resolveOption(root, options.out, SquireSchemas.DefaultOut)
    runSchemaReport(SquireSchemas.build(from, to, options.all), options.json, output, errorOutput)

  def runSchemasCompare(
      options: SchemasCompareOpts,
      root: Path,
      output: String => Unit,
      errorOutput: String => Unit
  ): Int < (Sync & Abort[SquireError]) =
    val from = resolveOption(root, options.from, SquireSchemas.DefaultFrom)
    val to   = options.out.fold(from)(value => resolvePath(root, value))
    runSchemaReport(SquireSchemas.compare(from, to, options.all), options.json, output, errorOutput)

  def runSchemasValidate(
      options: SchemasValidateOpts,
      root: Path,
      runner: ProcessRunner,
      output: String => Unit,
      errorOutput: String => Unit
  ): Int < (Async & Sync & Abort[SquireError]) =
    val sources    = root / SquireSchemas.DefaultFrom
    val schemas    = resolveOption(root, options.schemas, SquireSchemas.DefaultOut)
    val documents  = resolveOption(root, options.documents, SquireSchemas.DefaultDocuments)
    val jsonschema = Option(java.lang.System.getenv("SQUIRE_JSONSCHEMA_BIN")).filter(_.nonEmpty).getOrElse("jsonschema")
    runSchemaReport(
      SquireSchemas.validate(sources, schemas, documents, runner, jsonschema),
      options.json,
      output,
      errorOutput
    )

  private def runSchemaReport[S](
      operation: SchemaReport < (S & Abort[SquireError]),
      json: Boolean,
      output: String => Unit,
      errorOutput: String => Unit
  ): Int < (S & Abort[SquireError]) =
    operation.map { report =>
      output(if json then SquireJson.encode(report) + "\n" else SquireSchemas.renderText(report))
      SquireSchemas.exitCode(report)
    }

  private def resolveOption(root: Path, value: Option[String], default: String): Path =
    resolvePath(root, value.getOrElse(default))

  private def resolvePath(root: Path, value: String): Path =
    val path = Path(value)
    if path.toJava.isAbsolute then path else root / value

object SquireApp extends CommandsEntryPoint:
  override def progName: String = "squire"

  def commands = Seq(
    AiEnvInfoCmd,
    DoctorCmd,
    CellarGetCmd,
    CellarSearchCmd,
    CellarDepsCmd,
    ReferenceRepoAddCmd,
    ReferenceRepoListCmd,
    ReferenceRepoStatusCmd,
    ReferenceRepoRemoveCmd,
    BranchRefreshCmd,
    TrackingStatusCmd,
    TrackingSyncCmd,
    TrackingDoctorCmd,
    SpecSyncCmd,
    SpecExportCmd,
    SchemasBuildCmd,
    SchemasCompareCmd,
    SchemasValidateCmd
  )

  object AiEnvInfoCmd extends KyoCommand[AiEnvInfoOpts]:
    override def name  = "ai env info"
    override def names = List(List("ai", "env", "info"))
    run { options =>
      SquireCli.runCommand(
        SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
          SquireCli.runEnvInfo(options, root, SquireEnv.LivePlatform, java.lang.System.out.print)
        }
      )
    }

  object DoctorCmd extends KyoCommand[DoctorOpts]:
    override def name = "doctor"
    run { (_: DoctorOpts) =>
      SquireCli.runCommand(
        SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
          SquireDoctor.run(root, LiveProcessRunner, SquireEnv.LivePlatform).flatMap(SquireCli.printDoctor)
        }
      )
    }

  object CellarGetCmd extends KyoCommand[CellarGetOpts]:
    override def name  = "cellar get"
    override def names = List(List("cellar", "get"))
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

  object CellarSearchCmd extends KyoCommand[CellarSearchOpts]:
    override def name  = "cellar search"
    override def names = List(List("cellar", "search"))
    run { (options, remaining) =>
      SquireCli.runCommand(
        SquireCli.resolveRequiredArguments(
          "cellar search",
          List("coordinate" -> options.coordinate, "query" -> options.query),
          remaining.remaining,
          remaining.unparsed
        ).flatMap { values =>
          val coordinate :: query :: Nil = values: @unchecked
          SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
            SquireCli.runCellar(
              CellarAction.Search(coordinate, query, options.limit),
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

  object CellarDepsCmd extends KyoCommand[CellarDepsOpts]:
    override def name  = "cellar deps"
    override def names = List(List("cellar", "deps"))
    run { (options, remaining) =>
      SquireCli.runCommand(
        SquireCli.resolveRequiredArguments(
          "cellar deps",
          List("coordinate" -> options.coordinate),
          remaining.remaining,
          remaining.unparsed
        ).flatMap { values =>
          val coordinate :: Nil = values: @unchecked
          SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
            SquireCli.runCellar(
              CellarAction.Deps(coordinate),
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

  object ReferenceRepoAddCmd extends KyoCommand[ReferenceRepoAddOpts]:
    override def name  = "reference repo add"
    override def names = List(List("reference", "repo", "add"))
    run { (options, remaining) =>
      SquireCli.runCommand(
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
      )
    }

  object ReferenceRepoListCmd extends KyoCommand[ReferenceRepoListOpts]:
    override def name  = "reference repo list"
    override def names = List(List("reference", "repo", "list"))
    run { options =>
      SquireCli.runCommand(
        SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
          SquireCli.runReferenceList(options, root, LiveProcessRunner, java.lang.System.out.print)
        }
      )
    }

  object ReferenceRepoStatusCmd extends KyoCommand[ReferenceRepoStatusOpts]:
    override def name  = "reference repo status"
    override def names = List(List("reference", "repo", "status"))
    run { (options, remaining) =>
      SquireCli.runCommand(
        SquireCli.resolveOptionalArgument(
          "reference repo status",
          "name",
          options.name,
          remaining.remaining,
          remaining.unparsed
        ).flatMap { name =>
          SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
            SquireCli.runReferenceStatus(name, root, LiveProcessRunner, java.lang.System.out.print)
          }
        }
      )
    }

  object ReferenceRepoRemoveCmd extends KyoCommand[ReferenceRepoRemoveOpts]:
    override def name  = "reference repo remove"
    override def names = List(List("reference", "repo", "remove"))
    run { (options, remaining) =>
      SquireCli.runCommand(
        SquireCli.resolveRequiredArguments(
          "reference repo remove",
          List("name" -> options.name),
          remaining.remaining,
          remaining.unparsed
        ).flatMap { values =>
          val name :: Nil = values: @unchecked
          SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
            SquireCli.runReferenceRemove(
              name,
              options.keepFiles,
              root,
              LiveProcessRunner,
              LiveSquirePlatform,
              java.lang.System.out.print
            )
          }
        }
      )
    }

  object BranchRefreshCmd extends KyoCommand[BranchRefreshOpts]:
    override def name  = "branch refresh"
    override def names = List(List("branch", "refresh"))
    run { (options, remaining) =>
      SquireCli.runCommand(
        SquireCli.runBranch(options, remaining.all, LiveProcessRunner, java.lang.System.out.print)
      )
    }

  object TrackingStatusCmd extends KyoCommand[TrackingStatusOpts]:
    override def name  = "tracking status"
    override def names = List(List("tracking", "status"))
    run { options =>
      SquireCli.runCommand(
        SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
          SquireCli.runTrackingStatus(
            options,
            root,
            LiveProcessRunner,
            LiveSquirePlatform,
            java.lang.System.out.print
          )
        }
      )
    }

  object TrackingSyncCmd extends KyoCommand[TrackingSyncOpts]:
    override def name  = "tracking sync"
    override def names = List(List("tracking", "sync"))
    run { options =>
      SquireCli.runCommand(
        SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
          SquireCli.runTrackingSync(options, root, java.lang.System.out.print)
        }
      )
    }

  object TrackingDoctorCmd extends KyoCommand[TrackingDoctorOpts]:
    override def name  = "tracking doctor"
    override def names = List(List("tracking", "doctor"))
    run { (_: TrackingDoctorOpts) =>
      SquireCli.runCommand(
        SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
          SquireCli.runTrackingDoctor(
            root,
            LiveProcessRunner,
            LiveSquirePlatform,
            java.lang.System.out.print
          )
        }
      )
    }

  object SpecSyncCmd extends KyoCommand[SpecSyncOpts]:
    override def name  = "spec sync"
    override def names = List(List("spec", "sync"))
    run { options =>
      SquireCli.runCommand(
        SquireSpec.findRepoRoot(Path(java.lang.System.getProperty("user.dir")), LiveSquireSpecPlatform).flatMap {
          case Present(root) =>
            SquireCli.runSpecSync(
              options,
              root,
              LiveProcessRunner,
              LiveSquireSpecPlatform,
              java.lang.System.out.print,
              java.lang.System.err.print
            )
          case Absent => Abort.fail(SquireError.Failure("spec", "no repository root contains .claude/skills/kb/kb"))
        }
      )
    }

  object SpecExportCmd extends KyoCommand[SpecExportOpts]:
    override def name  = "spec export"
    override def names = List(List("spec", "export"))
    run { options =>
      SquireCli.runCommand(
        SquireSpec.findRepoRoot(Path(java.lang.System.getProperty("user.dir")), LiveSquireSpecPlatform).flatMap {
          case Present(root) =>
            SquireCli.runSpecExport(
              options,
              root,
              LiveProcessRunner,
              LiveSquireSpecPlatform,
              java.lang.System.out.print,
              java.lang.System.err.print
            )
          case Absent => Abort.fail(SquireError.Failure("spec", "no repository root contains .claude/skills/kb/kb"))
        }
      )
    }

  object SchemasBuildCmd extends KyoCommand[SchemasBuildOpts]:
    override def name  = "schemas build"
    override def names = List(List("schemas", "build"))
    run { options =>
      SquireCli.runCommand(
        SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
          SquireCli.runSchemasBuild(options, root, java.lang.System.out.print, java.lang.System.err.print)
        }
      )
    }

  object SchemasCompareCmd extends KyoCommand[SchemasCompareOpts]:
    override def name  = "schemas compare"
    override def names = List(List("schemas", "compare"))
    run { options =>
      SquireCli.runCommand(
        SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
          SquireCli.runSchemasCompare(options, root, java.lang.System.out.print, java.lang.System.err.print)
        }
      )
    }

  object SchemasValidateCmd extends KyoCommand[SchemasValidateOpts]:
    override def name  = "schemas validate"
    override def names = List(List("schemas", "validate"))
    run { options =>
      SquireCli.runCommand(
        SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
          SquireCli.runSchemasValidate(
            options,
            root,
            LiveProcessRunner,
            java.lang.System.out.print,
            java.lang.System.err.print
          )
        }
      )
    }
