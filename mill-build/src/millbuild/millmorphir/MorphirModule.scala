package millbuild.millmorphir

import millbuild.util.Collections._ 
import millbuild.util.ProcessHelper
import millbuild.jsruntime.JsRuntime 
import millbuild.millmorphir.api.{MakeArgs, MakeResult, MorphirProjectConfig}
import mill._
import mill.api.JsonFormatters._
import mill.scalalib._
import upickle.default._

trait MorphirModule extends Module { self =>
    /// Use indentation in the generated JSON file.
    def indentJson: Target[Boolean] = T(false)

    def morphirCommand:    Target[String]  = T("morphir")
    def morphirProjectDir: Target[PathRef] = T.source(PathRef(millSourcePath))

    def morphirProjectConfig: Target[MorphirProjectConfig] = T {
        val morphirProjectFile = morphirProjectDir().path / "morphir.json"
        if (os.exists(morphirProjectFile)) {
            read[MorphirProjectConfig](os.read(morphirProjectFile))
        } else {
            throw new Exception(s"morphir.json file not found, looked for it at ${morphirProjectFile}.")
        }
    }

    def makeCommandRunner: Target[String] = T {
        ProcessHelper.whereIs(morphirCommand())
    }

    def morphirIrFilename = T("morphir-ir.json")

    def makeArgs: Task[MakeArgs] = T.task {
        MakeArgs(
            projectDir = morphirProjectDir().path,
            output = T.dest / morphirIrFilename(),
            indentJson = indentJson(),
            typesOnly = typesOnly(),
            fallbackCli = None,
        )
    }

    def morphirMake: Target[MakeResult] = T {
        val makeArgs: MakeArgs = self.makeArgs()
        val cli = makeCommandRunner()

        //TODO: Rather than assuming this is a JS file check the header

        val commandArgs = makeArgs.toCommandArgs(cli)
        val workingDir  = T.ctx().dest
        val destPath    = makeArgs.output
        util.Jvm.runSubprocess(commandArgs, T.ctx().env, workingDir)
        MakeResult(makeArgs, PathRef(destPath), commandArgs, workingDir)
    }

    /// Only include type information in the IR, no values.
    def typesOnly: Target[Boolean] = T(false)

}