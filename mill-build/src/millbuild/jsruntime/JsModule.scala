package millbuild.jsruntime

import mill.*
import mill.scalalib.*

trait JsModule extends Module {
  def artifactName: T[String]           = Task(moduleDir.last)
  def segments: T[Seq[String]]          = Task(moduleDir.segments.toSeq.map(_.toString))
  def jsRunnerExecutable                = Task(PathRef(os.Path(JsRuntime.NodeJs.executable)))
  def jsPackageManagerRunner: T[String] = Task("npx")
  def jsPackageManagerCmd: T[String]    = Task("npm")

  /**
   * The folders containing all source files fed into the compiler
   */
  def allSources: T[Seq[PathRef]] = Task(sources() ++ generatedSources())

  /**
   * Folders containing source files that are generated rather than hand-written; these files can be generated in this
   * target itself, or can refer to files generated from other targets
   */
  def generatedSources: T[Seq[PathRef]] = Task(Seq.empty[PathRef])
  def sources: T[Seq[PathRef]]          = Task(Seq(PathRef(moduleDir / "src")))

  /**
   * All individual source files fed into the compiler/tooling.
   */
  def allSourceFiles: T[Seq[PathRef]] = Task {
    Lib.findSourceFiles(allSources(), Seq("js", "ts")).map(PathRef(_))
  }

}
