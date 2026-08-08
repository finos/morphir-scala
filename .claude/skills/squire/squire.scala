//| scalaVersion: 3.8.4
//| mainClass: SquireApp
//| moduleDeps: [SquireModel.scala, SquireProcess.scala]
//| mvnDeps:
//| - io.getkyo::kyo-case-app:1.0.0-RC6

import caseapp.*
import caseapp.core.app.CommandsEntryPoint
import kyo.*

case class AiEnvInfoOpts(
    @HelpMessage("Run one named check and return its status") check: Option[String] = None,
    @HelpMessage("Bound live probes in seconds") timeout: Int = 8
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

case class ReferenceRepoListOpts()
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
    run { (_: AiEnvInfoOpts) => SquireCli.notImplemented(name) }

  object DoctorCmd extends KyoCommand[DoctorOpts]:
    override def name = "doctor"
    run { (_: DoctorOpts) => SquireCli.notImplemented(name) }

  object CellarGetCmd extends KyoCommand[CellarGetOpts]:
    override def name = "cellar get"
    override def names = List(List("cellar", "get"))
    run { (_: CellarGetOpts) => SquireCli.notImplemented(name) }

  object CellarSearchCmd extends KyoCommand[CellarSearchOpts]:
    override def name = "cellar search"
    override def names = List(List("cellar", "search"))
    run { (_: CellarSearchOpts) => SquireCli.notImplemented(name) }

  object CellarDepsCmd extends KyoCommand[CellarDepsOpts]:
    override def name = "cellar deps"
    override def names = List(List("cellar", "deps"))
    run { (_: CellarDepsOpts) => SquireCli.notImplemented(name) }

  object ReferenceRepoAddCmd extends KyoCommand[ReferenceRepoAddOpts]:
    override def name = "reference repo add"
    override def names = List(List("reference", "repo", "add"))
    run { (_: ReferenceRepoAddOpts) => SquireCli.notImplemented(name) }

  object ReferenceRepoListCmd extends KyoCommand[ReferenceRepoListOpts]:
    override def name = "reference repo list"
    override def names = List(List("reference", "repo", "list"))
    run { (_: ReferenceRepoListOpts) => SquireCli.notImplemented(name) }

  object ReferenceRepoStatusCmd extends KyoCommand[ReferenceRepoStatusOpts]:
    override def name = "reference repo status"
    override def names = List(List("reference", "repo", "status"))
    run { (_: ReferenceRepoStatusOpts) => SquireCli.notImplemented(name) }

  object ReferenceRepoRemoveCmd extends KyoCommand[ReferenceRepoRemoveOpts]:
    override def name = "reference repo remove"
    override def names = List(List("reference", "repo", "remove"))
    run { (_: ReferenceRepoRemoveOpts) => SquireCli.notImplemented(name) }

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
