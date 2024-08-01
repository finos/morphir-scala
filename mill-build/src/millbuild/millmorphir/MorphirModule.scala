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

    def clean() = T.command {
        var pendingDelete = List(morphirProjectDirResolved().path / "morphir-hashes.json")
        pendingDelete.foreach { path =>
            if (os.exists(path)) {
                os.remove.all(path)
            }
        }
    }
    
    def dist = T {
        val makeOutput = make()

        val distPath = distFolder().path
        if (!os.exists(distPath)) {
            os.makeDir.all(distPath)
        }

        val paths = makeOutput.values.flatMap(a => a.map(_.path))
        paths.foreach { path =>
            val targetDir = distPath
            if (!os.exists(targetDir)) {
                os.makeDir.all(targetDir)
            }

            val targetPath = targetDir / path.last

            T.ctx().log.info(s"Copying ${path} to $targetPath")
            try {
                os.copy.over(path, targetPath)
            } catch {
                case e: Exception =>
                    T.ctx().log.error(s"Failed to copy $path to $targetPath")
                    T.ctx().log.error(e.toString)
            }
        
        }
        paths.map(PathRef(_))
    }

    def distFolder:Target[PathRef] = T {
        PathRef(morphirProjectDirResolved().path / "dist")
    }

    /// Use indentation in the generated JSON file.
    def indentJson: Target[Boolean] = T(false)

    def morphirCommand:    Target[String]  = T("morphir")
    def morphirProjectDir: Target[PathRef] = T.source(PathRef(millSourcePath))
    final def morphirProjectDirResolved: Target[PathRef] = T {
        PathRef(makeArgs().projectDir)
    }
    def morphirHashesPath: Target[PathRef] = T {
        PathRef(morphirProjectDir().path / "morphir-hashes.json")
    }

    def morphirHashesContent: Target[Option[ujson.Value]] = T {
        if (os.exists(morphirProjectDirResolved().path)) {
            Option(ujson.read(os.read(morphirHashesPath().path)))
        } else {
            None
        }
    }

    def sources = T.sources { Seq(PathRef(millSourcePath / "src")) }

    def allSourceFiles: T[Seq[PathRef]] = T {
        sources().map(_.path).flatMap(os.walk(_).filter(_.toIO.isFile)).map(PathRef(_))
    }

    def morphirIncrementalBuildSourceFiles = T.sources {
        Seq(PathRef(morphirProjectDir().path / "morphir-hashes.json"))
    }

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

    def make:Target[Map[String, List[PathRef]]] = T {
        val makeResult = morphirMake()
        Map("ir" -> List(makeResult.irFilePath)) ++ makeResult.morphirHashesPath.map { path =>
            Map("compiler.out" -> List(path))
        }.getOrElse(Map.empty)
    }



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

        val _ = allSourceFiles()
        val commandArgs = makeArgs.toCommandArgs(cli)
        val workingDir  = makeArgs.projectDir
        val destPath    = makeArgs.output
        util.Jvm.runSubprocess(commandArgs, T.ctx().env, workingDir)
        val hashesPath = morphirHashesPath()
        val hashesPathFinal =
            if(os.exists(hashesPath.path)) Option(hashesPath) else None
        MakeResult(makeArgs, PathRef(destPath), commandArgs, workingDir, morphirHashesPath = hashesPathFinal)
    }

    /// Only include type information in the IR, no values.
    def typesOnly: Target[Boolean] = T(false)

}