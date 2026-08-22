package morphir.desktop.main

import kyo.*
import morphir.ui.services.*

/**
 * Fixed demo read models for the proof-of-stack release. Real workspace-backed services replace these under a later
 * intent; the contract stays.
 */
object DemoServices:

  val ir: IrService = new IrService:
    def listPackages(workspace: WorkspaceRef) =
      Chunk(PackageInfo("Morphir.SDK", 3), PackageInfo("Acme.Models", 1))
    def listModules(workspace: WorkspaceRef, packageName: String) =
      packageName match
        case "Morphir.SDK" =>
          Chunk(
            ModuleInfo(packageName, "List", 4, 21),
            ModuleInfo(packageName, "Maybe", 1, 9),
            ModuleInfo(packageName, "Result", 1, 11)
          )
        case other => Chunk(ModuleInfo(other, "Main", 2, 5))
    def definition(workspace: WorkspaceRef, ref: DefinitionRef) =
      DefinitionDetail(ref, DefinitionKind.Value, s"${ref.localName} : <demo signature>")

  val knowledge: KnowledgeService = new KnowledgeService:
    def listBundles(workspace: WorkspaceRef) =
      Chunk(BundleInfo("intent", "Intent", 30), BundleInfo("morphir/morphir-scala", "morphir-scala", 40))
    def concept(workspace: WorkspaceRef, ref: ConceptRef) =
      ConceptDetail(ref, "Intent", ref.path, "Demo concept body.")
    def intentIndex(workspace: WorkspaceRef) =
      Chunk(
        IntentSummary("0025", "Electron appkit", "InProgress", "feature"),
        IntentSummary("0029", "morphir-ui kyo-ui client library", "InProgress", "feature"),
        IntentSummary("0030", "morphir-desktop Electron app", "InProgress", "feature")
      )

  def shell(version: String): ShellService = new ShellService:
    def pickWorkspace()    = Absent
    def recentWorkspaces() = Chunk(WorkspaceRef("/demo"))
    def appVersion()       = version

  def routes(version: String, github: GitHubConnectionService): Chunk[JsonRpcRoute[?, ?, ?]] =
    IrRpc.routes(ir) ++ KnowledgeRpc.routes(knowledge) ++ ShellRpc.routes(shell(version)) ++
      GitHubConnectionRpc.routes(github)
