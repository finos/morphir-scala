//| scalaVersion: 3.8.4
//| mainClass: SquireApp
//| moduleDeps: [SquireModel.scala, SquireProcess.scala, SquireEnv.scala, SquireDoctor.scala, SquireCellar.scala, SquireRepo.scala]
//| mvnDeps:
//| - io.getkyo::kyo-case-app:1.0.0-RC6

import caseapp.*
import caseapp.core.app.CommandsEntryPoint
import kyo.*

case class AiEnvInfoOpts(
    @HelpMessage("Run one named check and return its status") check: Option[String] = None,
    @HelpMessage("Bound live probes in seconds") timeout: Double = 8.0
)

case class DoctorOpts()

case class CellarGetOpts(
    @HelpMessage("Maven coordinate or project alias") coordinate: String,
    @HelpMessage("Fully qualified symbol") symbol: String,
    @HelpMessage("Hide inherited members") hideInherited: Boolean = false,
    @HelpMessage("Group inherited members") groupInherited: Boolean = false,
    @HelpMessage("Maximum number of results") limit: Option[Int] = None
)

case class CellarSearchOpts(
    @HelpMessage("Maven coordinate or project alias") coordinate: String,
    @HelpMessage("Symbol-name substring") query: String,
    @HelpMessage("Maximum number of results") limit: Option[Int] = None
)

case class CellarDepsOpts(@HelpMessage("Maven coordinate or project alias") coordinate: String)

case class ReferenceRepoAddOpts(
    @HelpMessage("Git URL or local repository path") urlOrPath: String,
    @HelpMessage("Override the repository name") name: Option[String] = None,
    @HelpMessage("Branch, tag, or commit to check out") ref: Option[String] = None,
    @HelpMessage("Reference repository strategy") strategy: String = "clone",
    @HelpMessage("Shallow clone depth") depth: Option[Int] = None,
    @HelpMessage("Clone complete history") full: Boolean = false,
    @HelpMessage("Subtrees to materialise") sparse: List[String] = Nil
)

case class ReferenceRepoListOpts(@HelpMessage("Output the raw manifest as JSON") json: Boolean = false)
case class ReferenceRepoStatusOpts(@HelpMessage("Repository name") name: Option[String] = None)
case class ReferenceRepoRemoveOpts(
    @HelpMessage("Repository name") name: String,
    @HelpMessage("Leave checkout files in place") keepFiles: Boolean = false
)

case class BranchRefreshOpts(
    @HelpMessage("Target branch to refresh") target: String = "develop",
    @HelpMessage("Report the update without writing") dryRun: Boolean = false
)

case class TrackingStatusOpts()
case class TrackingSyncOpts()
case class TrackingDoctorOpts()

case class SpecSyncOpts(
    @HelpMessage("Report changes without writing") dryRun: Boolean = false,
    @HelpMessage("Upstream ref to import") ref: Option[String] = None,
    @HelpMessage("Take the upstream side of conflicts") theirs: Boolean = false,
    @HelpMessage("Remove files deleted upstream") prune: Boolean = false,
    @HelpMessage("Use the checkout without fetching") noFetch: Boolean = false
)

case class SpecExportOpts(
    @HelpMessage("Target Morphir checkout") to: Option[String] = None,
    @HelpMessage("Report changes without writing") dryRun: Boolean = false,
    @HelpMessage("Include changes that also moved upstream") includeDiverged: Boolean = false,
    @HelpMessage("Branch for exported changes") branch: String = "morphir-kb/spec-sync",
    @HelpMessage("Do not create or switch a branch") noBranch: Boolean = false
)

case class SchemasBuildOpts(
    @HelpMessage("Input schema directory") from: Option[String] = None,
    @HelpMessage("Output schema directory") out: Option[String] = None
)

case class SchemasCompareOpts(
    @HelpMessage("Input schema directory") from: Option[String] = None,
    @HelpMessage("Output schema directory") out: Option[String] = None
)

case class SchemasValidateOpts(
    @HelpMessage("Schema directory") schemas: Option[String] = None,
    @HelpMessage("Document directory") documents: Option[String] = None
)

object SquireCli:
  def notImplemented(command: String): Unit < Sync =
    Sync.defer {
      java.lang.System.err.println(s"error: squire $command is not implemented yet")
      java.lang.System.exit(1)
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
      case Some("jvm-network") => SquireEnv.check(SquireEnv.CheckKind.JvmNetwork, timeoutDuration(options.timeout), platform).map(if _ then 0 else 1)
      case Some("var-folders") => SquireEnv.check(SquireEnv.CheckKind.VarFolders, timeoutDuration(options.timeout), platform).map(if _ then 0 else 1)
      case Some(_)              => 2

  def printDoctor(report: SquireDoctor.DoctorReport): Unit < Sync =
    Sync.defer(report.findings.foreach(finding => java.lang.System.out.println(s"${finding.code} - ${finding.message}")))

  def runCellar(
      action: CellarAction,
      root: Path,
      runner: ProcessRunner,
      platform: SquirePlatform,
      output: String => Unit,
      errorOutput: String => Unit
  ): Int < (Async & Abort[SquireError]) =
    SquireCellar.run(action, root, runner, platform).map { result =>
      if result.stdout.nonEmpty then output(result.stdout)
      if result.stderr.nonEmpty then errorOutput(result.stderr)
      result.exitCode
    }

  def runReferenceAdd(
      options: ReferenceRepoAddOpts,
      root: Path,
      runner: ProcessRunner,
      platform: SquirePlatform,
      output: String => Unit
  ): Int < (Async & Sync & Abort[SquireError]) =
    SquireRepo
      .add(
        ReferenceAdd(
          options.urlOrPath,
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
      options: ReferenceRepoStatusOpts,
      root: Path,
      runner: ProcessRunner,
      output: String => Unit
  ): Int < (Async & Sync & Abort[SquireError]) =
    SquireRepo.status(root, options.name, runner).map { report =>
      output(report.output)
      report.exitCode
    }

  def runReferenceRemove(
      options: ReferenceRepoRemoveOpts,
      root: Path,
      runner: ProcessRunner,
      platform: SquirePlatform,
      output: String => Unit
  ): Int < (Async & Sync & Abort[SquireError]) =
    SquireRepo.remove(options.name, options.keepFiles, root, runner, platform).map { _ =>
      output(s"Removed '${options.name}' from manifest.\n")
      0
    }

  def exitUnlessZero(exitCode: Int): Unit < Sync =
    if exitCode == 0 then () else Sync.defer(java.lang.System.exit(exitCode))

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
    override def name = "ai env info"
    override def names = List(List("ai", "env", "info"))
    run { options =>
      SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
        SquireCli
          .runEnvInfo(options, root, SquireEnv.LivePlatform, java.lang.System.out.print)
        .flatMap { exitCode =>
          if exitCode == 0 then ()
          else Sync.defer(java.lang.System.exit(exitCode))
        }
      }
    }

  object DoctorCmd extends KyoCommand[DoctorOpts]:
    override def name = "doctor"
    run { (_: DoctorOpts) =>
      SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
        SquireDoctor
          .run(root, LiveProcessRunner, SquireEnv.LivePlatform)
          .flatMap(SquireCli.printDoctor)
      }
    }

  object CellarGetCmd extends KyoCommand[CellarGetOpts]:
    override def name  = "cellar get"
    override def names = List(List("cellar", "get"))
    run { options =>
      SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
        SquireCli
          .runCellar(
            CellarAction.Get(
              options.coordinate,
              options.symbol,
              options.hideInherited,
              options.groupInherited,
              options.limit
            ),
            root,
            LiveProcessRunner,
            LiveSquirePlatform,
            java.lang.System.out.print,
            java.lang.System.err.print
          )
          .flatMap(SquireCli.exitUnlessZero)
      }
    }

  object CellarSearchCmd extends KyoCommand[CellarSearchOpts]:
    override def name  = "cellar search"
    override def names = List(List("cellar", "search"))
    run { options =>
      SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
        SquireCli
          .runCellar(
            CellarAction.Search(options.coordinate, options.query, options.limit),
            root,
            LiveProcessRunner,
            LiveSquirePlatform,
            java.lang.System.out.print,
            java.lang.System.err.print
          )
          .flatMap(SquireCli.exitUnlessZero)
      }
    }

  object CellarDepsCmd extends KyoCommand[CellarDepsOpts]:
    override def name  = "cellar deps"
    override def names = List(List("cellar", "deps"))
    run { options =>
      SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
        SquireCli
          .runCellar(
            CellarAction.Deps(options.coordinate),
            root,
            LiveProcessRunner,
            LiveSquirePlatform,
            java.lang.System.out.print,
            java.lang.System.err.print
          )
          .flatMap(SquireCli.exitUnlessZero)
      }
    }

  object ReferenceRepoAddCmd extends KyoCommand[ReferenceRepoAddOpts]:
    override def name  = "reference repo add"
    override def names = List(List("reference", "repo", "add"))
    run { options =>
      SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
        SquireCli
          .runReferenceAdd(options, root, LiveProcessRunner, LiveSquirePlatform, java.lang.System.out.print)
          .flatMap(SquireCli.exitUnlessZero)
      }
    }

  object ReferenceRepoListCmd extends KyoCommand[ReferenceRepoListOpts]:
    override def name  = "reference repo list"
    override def names = List(List("reference", "repo", "list"))
    run { options =>
      SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
        SquireCli
          .runReferenceList(options, root, LiveProcessRunner, java.lang.System.out.print)
          .flatMap(SquireCli.exitUnlessZero)
      }
    }

  object ReferenceRepoStatusCmd extends KyoCommand[ReferenceRepoStatusOpts]:
    override def name  = "reference repo status"
    override def names = List(List("reference", "repo", "status"))
    run { options =>
      SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
        SquireCli
          .runReferenceStatus(options, root, LiveProcessRunner, java.lang.System.out.print)
          .flatMap(SquireCli.exitUnlessZero)
      }
    }

  object ReferenceRepoRemoveCmd extends KyoCommand[ReferenceRepoRemoveOpts]:
    override def name  = "reference repo remove"
    override def names = List(List("reference", "repo", "remove"))
    run { options =>
      SquireCli.projectRoot(Path(java.lang.System.getProperty("user.dir"))).flatMap { root =>
        SquireCli
          .runReferenceRemove(options, root, LiveProcessRunner, LiveSquirePlatform, java.lang.System.out.print)
          .flatMap(SquireCli.exitUnlessZero)
      }
    }

  object BranchRefreshCmd extends KyoCommand[BranchRefreshOpts]:
    override def name = "branch refresh"
    override def names = List(List("branch", "refresh"))
    run { (_: BranchRefreshOpts) => SquireCli.notImplemented(name) }

  object TrackingStatusCmd extends KyoCommand[TrackingStatusOpts]:
    override def name = "tracking status"
    override def names = List(List("tracking", "status"))
    run { (_: TrackingStatusOpts) => SquireCli.notImplemented(name) }

  object TrackingSyncCmd extends KyoCommand[TrackingSyncOpts]:
    override def name = "tracking sync"
    override def names = List(List("tracking", "sync"))
    run { (_: TrackingSyncOpts) => SquireCli.notImplemented(name) }

  object TrackingDoctorCmd extends KyoCommand[TrackingDoctorOpts]:
    override def name = "tracking doctor"
    override def names = List(List("tracking", "doctor"))
    run { (_: TrackingDoctorOpts) => SquireCli.notImplemented(name) }

  object SpecSyncCmd extends KyoCommand[SpecSyncOpts]:
    override def name = "spec sync"
    override def names = List(List("spec", "sync"))
    run { (_: SpecSyncOpts) => SquireCli.notImplemented(name) }

  object SpecExportCmd extends KyoCommand[SpecExportOpts]:
    override def name = "spec export"
    override def names = List(List("spec", "export"))
    run { (_: SpecExportOpts) => SquireCli.notImplemented(name) }

  object SchemasBuildCmd extends KyoCommand[SchemasBuildOpts]:
    override def name = "schemas build"
    override def names = List(List("schemas", "build"))
    run { (_: SchemasBuildOpts) => SquireCli.notImplemented(name) }

  object SchemasCompareCmd extends KyoCommand[SchemasCompareOpts]:
    override def name = "schemas compare"
    override def names = List(List("schemas", "compare"))
    run { (_: SchemasCompareOpts) => SquireCli.notImplemented(name) }

  object SchemasValidateCmd extends KyoCommand[SchemasValidateOpts]:
    override def name = "schemas validate"
    override def names = List(List("schemas", "validate"))
    run { (_: SchemasValidateOpts) => SquireCli.notImplemented(name) }
