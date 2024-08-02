package millbuild.morphirlib

import millbuild.util.Collections._ 
import millbuild.util.ProcessHelper
import millbuild.jsruntime.JsRuntime 
import millbuild.morphirlib.api._
import mill._
import mill.define.Segment
import mill.api.JsonFormatters._
import mill.scalalib._
import mill.scalalib.internal.ModuleUtils
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
        val artifacts = make()

        val distPath = distFolder().path
        if (!os.exists(distPath)) {
            os.makeDir.all(distPath)
        }
        
        artifacts.map { artifact =>
            val path = artifact.path
            val targetDir = distPath
            if (!os.exists(targetDir)) {
                os.makeDir.all(targetDir)
            }

            val targetPath = targetDir / path.last

            T.ctx().log.debug(s"Copying ${path} to $targetPath")
            try {
                os.copy.over(path, targetPath)
                Option(artifact.withPath(targetPath))
            } catch {
                case e: Exception =>
                    T.ctx().log.error(s"Failed to copy $path to $targetPath")
                    T.ctx().log.error(e.toString)
                    None
            }
                        
        }.collect { case Some(artifact) => artifact }.distinctBy(_.path)        
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

    def make:Target[Seq[ArtifactRef]] = T {
        val makeResult = morphirMake()
        Seq(ArtifactRef.morphirIR(makeResult.irFilePath, "morphir", "ir")) ++ makeResult.morphirHashesPath.map { path =>
            Seq(ArtifactRef.morphirHashes(path, "morphir", "hashes","incremental"))
        }.getOrElse(Seq.empty)
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

    /**
      * The direct dependencies of this module.
      * This is meant to be overridden to add dependencies.
      * To read the value, you should use [[morphirModuleDepsChecked]] instead,
      * which uses a cached result which is also checked to be free of cycles.
      * @see [[morphirModuleDepsChecked]]
      */
    def morphirModuleDeps: Seq[MorphirModule] = Seq.empty

    /**
      * Same as [[morphirModuleDeps]], but checked to not contain cycles.
      * Prefer this over using [[moduleDeps]] directly.
      */
    final def morphirModuleDepsChecked: Seq[MorphirModule] = {
        recMorphirModuleDeps
        morphirModuleDeps
    }

    /** Should only be called from [[moduleDepsChecked]] */
    private lazy val recMorphirModuleDeps: Seq[MorphirModule] =
        ModuleUtils.recursive[MorphirModule](
            (millModuleSegments ++ Seq(Segment.Label("morphirModuleDeps"))).render,
            this,
            _.morphirModuleDeps
        )

    /** The direct and indirect dependencies of this module */
    def recursiveMorphirModuleDeps: Seq[MorphirModule] = {
        recMorphirModuleDeps 
    }

    /**
      * Like `recursiveMorphirModuleDeps`, but includes this module itself as well.
      */
    def transitiveMorphirModuleDeps:Seq[MorphirModule] = Seq(this) ++ recursiveMorphirModuleDeps

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