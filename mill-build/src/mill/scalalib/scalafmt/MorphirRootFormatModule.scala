package mill.scalalib.scalafmt

import mainargs.{Flag, arg}
import mill.*
import mill.api.{BuildCtx, DefaultTaskModule, Evaluator, PathRef, Result, SelectMode, TaskCtx}
import millbuild.{FormatKind, FormatSelection}
import org.finos.millmorphir.elm.ElmFormatToolModule

/**
 * Root `./mill format` implementation. Lives in `mill.scalalib.scalafmt` so [[ScalafmtWorker]] (`private[scalafmt]`) is
 * visible inside [[Task.Command]] bodies.
 *
 * Repo wiring: `format/` extends this via `build.format.MorphirFormatModule` and sets [[elmFormatTool]].
 *
 * Mill requires `Task#apply` lexically inside `Task` / `Command` braces — `ScalafmtWorkerModule.worker()` and
 * `elmFormatExecutable()` therefore appear only in the command method bodies below (not in private helpers; even
 * `inline def` is typechecked at the definition site).
 */
trait MorphirRootFormatModule extends Module, DefaultTaskModule {

  def defaultTask(): String = "format"

  /** Shared toolchains elm-format install (repo sets this to `toolchains.elmFormat`). */
  def elmFormatTool: ElmFormatToolModule

  /**
   * Format or check according to kind and selection flags.
   *
   * Dispatch: `--paths` / `--changed` → route by extension; else `--sources` → scalafmt selector; else full sweep
   * (`__.sources` + build `.mill` + Elm roots).
   */
  def format(
      evaluator: Evaluator,
      kind: String = "all",
      @arg(name = "paths") paths: Seq[String] = Seq.empty,
      changed: Flag = Flag(),
      sources: String = "",
      check: Flag = Flag()
  ) = Task.Command(exclusive = true) {
    val parsed    = FormatKind.parse(kind).fold(msg => throw new Exception(s"format: $msg"), identity)
    val checkMode = check.value
    if paths.nonEmpty || changed.value then {
      val (scalaFiles, elmFiles) = selectPathModeFiles(parsed, paths, changed.value)
      val scalaExisting          = scalaFiles.filter(os.isFile).distinct
      if scalaExisting.nonEmpty then {
        val refs = scalaExisting.map(PathRef(_))
        val cfg  = scalafmtConfigPathRef()
        Task.log.info(
          s"format: ${if checkMode then "checking" else "formatting"} ${refs.size} scala/.mill path(s) via ScalafmtWorker"
        )
        if checkMode then
          ScalafmtWorkerModule.worker().checkFormat(refs, cfg).toEither match {
            case Right(_)    => ()
            case Left(error) => throw new Exception(error)
          }
        else ScalafmtWorkerModule.worker().reformat(refs, cfg)
      }
      val elmExisting = elmFiles.filter(os.isFile).distinct.sorted
      if elmExisting.nonEmpty then {
        val command = elmFormatTool.elmFormatExecutable()
        invokeElmFormat(command.executable.path, command.arguments, elmExisting, checkMode)
      }
    } else if !sources.isBlank then {
      if parsed == FormatKind.Elm then
        throw new Exception("format: --sources is scalafmt-only; cannot combine with --kind elm")
      evaluateScalafmtSelector(evaluator, sources, checkMode)
    } else {
      val workspace = BuildCtx.workspaceRoot
      if FormatSelection.scalaExtensions(parsed) then {
        evaluateScalafmtSelector(evaluator, "__.sources", checkMode)
        val millFiles =
          FormatSelection.discoverBuildMillFiles(workspace).map(workspace / _).filter(os.isFile)
        if millFiles.nonEmpty then {
          val refs = millFiles.map(PathRef(_))
          val cfg  = scalafmtConfigPathRef()
          Task.log.info(
            s"format: ${if checkMode then "checking" else "formatting"} ${refs.size} build .mill file(s) via ScalafmtWorker"
          )
          if checkMode then
            ScalafmtWorkerModule.worker().checkFormat(refs, cfg).toEither match {
              case Right(_)    => ()
              case Left(error) => throw new Exception(error)
            }
          else ScalafmtWorkerModule.worker().reformat(refs, cfg)
        }
      }
      if FormatSelection.elmExtensions(parsed) then {
        val elmFiles = discoverElmSourceFiles(workspace)
        if elmFiles.nonEmpty then {
          Task.log.info(
            s"format: ${if checkMode then "checking" else "formatting"} ${elmFiles.size} Elm file(s)"
          )
          val command = elmFormatTool.elmFormatExecutable()
          invokeElmFormat(command.executable.path, command.arguments, elmFiles, checkMode)
        }
      }
    }
  }

  /** Same as `format --kind scala`. Invoke as `./mill format.scala`. */
  def scala(
      evaluator: Evaluator,
      @arg(name = "paths") paths: Seq[String] = Seq.empty,
      changed: Flag = Flag(),
      sources: String = "",
      check: Flag = Flag()
  ) = Task.Command(exclusive = true) {
    val checkMode = check.value
    if paths.nonEmpty || changed.value then {
      val (scalaFiles, _) = selectPathModeFiles(FormatKind.Scala, paths, changed.value)
      val scalaExisting   = scalaFiles.filter(os.isFile).distinct
      if scalaExisting.nonEmpty then {
        val refs = scalaExisting.map(PathRef(_))
        val cfg  = scalafmtConfigPathRef()
        Task.log.info(
          s"format: ${if checkMode then "checking" else "formatting"} ${refs.size} scala/.mill path(s) via ScalafmtWorker"
        )
        if checkMode then
          ScalafmtWorkerModule.worker().checkFormat(refs, cfg).toEither match {
            case Right(_)    => ()
            case Left(error) => throw new Exception(error)
          }
        else ScalafmtWorkerModule.worker().reformat(refs, cfg)
      }
    } else if !sources.isBlank then {
      evaluateScalafmtSelector(evaluator, sources, checkMode)
    } else {
      val workspace = BuildCtx.workspaceRoot
      evaluateScalafmtSelector(evaluator, "__.sources", checkMode)
      val millFiles =
        FormatSelection.discoverBuildMillFiles(workspace).map(workspace / _).filter(os.isFile)
      if millFiles.nonEmpty then {
        val refs = millFiles.map(PathRef(_))
        val cfg  = scalafmtConfigPathRef()
        Task.log.info(
          s"format: ${if checkMode then "checking" else "formatting"} ${refs.size} build .mill file(s) via ScalafmtWorker"
        )
        if checkMode then
          ScalafmtWorkerModule.worker().checkFormat(refs, cfg).toEither match {
            case Right(_)    => ()
            case Left(error) => throw new Exception(error)
          }
        else ScalafmtWorkerModule.worker().reformat(refs, cfg)
      }
    }
  }

  /** Same as `format --kind elm`. Invoke as `./mill format.elm`. */
  def elm(
      evaluator: Evaluator,
      @arg(name = "paths") paths: Seq[String] = Seq.empty,
      changed: Flag = Flag(),
      check: Flag = Flag()
  ) = Task.Command(exclusive = true) {
    val checkMode = check.value
    if paths.nonEmpty || changed.value then {
      val (_, elmFiles) = selectPathModeFiles(FormatKind.Elm, paths, changed.value)
      val elmExisting   = elmFiles.filter(os.isFile).distinct.sorted
      if elmExisting.nonEmpty then {
        val command = elmFormatTool.elmFormatExecutable()
        invokeElmFormat(command.executable.path, command.arguments, elmExisting, checkMode)
      }
    } else {
      val elmFiles = discoverElmSourceFiles(BuildCtx.workspaceRoot)
      if elmFiles.nonEmpty then {
        Task.log.info(
          s"format: ${if checkMode then "checking" else "formatting"} ${elmFiles.size} Elm file(s)"
        )
        val command = elmFormatTool.elmFormatExecutable()
        invokeElmFormat(command.executable.path, command.arguments, elmFiles, checkMode)
      }
    }
  }

  /**
   * Full-repo check (scala sources + build `.mill` + Elm). Prefer [[checkBuildAndElm]] from `ci.lint` when scala
   * modules are already checked with `--exclude`.
   */
  def checkAll(evaluator: Evaluator) = Task.Command(exclusive = true) {
    val workspace = BuildCtx.workspaceRoot
    evaluateScalafmtSelector(evaluator, "__.sources", checkMode = true)
    val millFiles =
      FormatSelection.discoverBuildMillFiles(workspace).map(workspace / _).filter(os.isFile)
    if millFiles.nonEmpty then {
      val refs = millFiles.map(PathRef(_))
      val cfg  = scalafmtConfigPathRef()
      Task.log.info(s"format: checking ${refs.size} build .mill file(s) via ScalafmtWorker")
      ScalafmtWorkerModule.worker().checkFormat(refs, cfg).toEither match {
        case Right(_)    => ()
        case Left(error) => throw new Exception(error)
      }
    }
    val elmFiles = discoverElmSourceFiles(workspace)
    if elmFiles.nonEmpty then {
      Task.log.info(s"format: checking ${elmFiles.size} Elm file(s)")
      val command = elmFormatTool.elmFormatExecutable()
      invokeElmFormat(command.executable.path, command.arguments, elmFiles, checkMode = true)
    }
  }

  /**
   * Check build `.mill` files and Elm sources only (same sets as full `./mill format --check`). Used by `ci.lint`
   * after the scala `checkFormatAll` loop so `--exclude` stays scala-only and scala is not double-checked.
   *
   * Mill requires `ScalafmtWorkerModule.worker()` / `elmFormatExecutable()` lexically inside this command body —
   * do not factor those calls into a shared helper.
   */
  def checkBuildAndElm() = Task.Command(exclusive = true) {
    val workspace = BuildCtx.workspaceRoot
    val millFiles =
      FormatSelection.discoverBuildMillFiles(workspace).map(workspace / _).filter(os.isFile)
    if millFiles.nonEmpty then {
      val refs = millFiles.map(PathRef(_))
      val cfg  = scalafmtConfigPathRef()
      Task.log.info(s"format: checking ${refs.size} build .mill file(s) via ScalafmtWorker")
      ScalafmtWorkerModule.worker().checkFormat(refs, cfg).toEither match {
        case Right(_)    => ()
        case Left(error) => throw new Exception(error)
      }
    }
    val elmFiles = discoverElmSourceFiles(workspace)
    if elmFiles.nonEmpty then {
      Task.log.info(s"format: checking ${elmFiles.size} Elm file(s)")
      val command = elmFormatTool.elmFormatExecutable()
      invokeElmFormat(command.executable.path, command.arguments, elmFiles, checkMode = true)
    }
  }

  private def selectPathModeFiles(
      kind: FormatKind,
      pathArgs: Seq[String],
      changed: Boolean
  )(using TaskCtx): (Seq[os.Path], Seq[os.Path]) = {
    val workspace = BuildCtx.workspaceRoot
    val fromArgs  = pathArgs.map(toWorkspaceRelPath(workspace, _))
    val fromGit   =
      if !changed then Seq.empty
      else {
        val porcelain =
          os.proc("git", "status", "--porcelain", "-uall")
            .call(cwd = workspace, stdout = os.Pipe)
            .out
            .text()
        FormatSelection.gitStatusPaths(porcelain)
      }
    val selected = FormatSelection.filterChanged((fromArgs ++ fromGit).distinct, kind)
    // Directories survive filterChanged (empty extension) and must expand before routeByExtension.
    val (dirRels, fileRels) = selected.partition(rel => os.isDir(workspace / rel))
    val routed              = FormatSelection.routeByExtension(fileRels)
    if routed.ignored.nonEmpty then
      Task.log.info(s"format: ignoring ${routed.ignored.size} non-format path(s)")
    val scalaDirs = if FormatSelection.scalaExtensions(kind) then dirRels else Seq.empty
    val elmDirs   = if FormatSelection.elmExtensions(kind) then dirRels else Seq.empty
    (
      expandScalaFiles(workspace, routed.scalaPaths ++ scalaDirs),
      expandElmFiles(workspace, routed.elmPaths ++ elmDirs)
    )
  }

  private def evaluateScalafmtSelector(
      evaluator: Evaluator,
      sourcesSelector: String,
      checkMode: Boolean
  )(using TaskCtx): Unit = {
    val task =
      if checkMode then "mill.scalalib.scalafmt.ScalafmtModule/checkFormatAll"
      else "mill.scalalib.scalafmt.ScalafmtModule/reformatAll"
    Task.log.info(s"format: $task $sourcesSelector")
    val evalResult = unwrap(
      evaluator.evaluate(Seq(task, sourcesSelector), SelectMode.Separated),
      s"format $task"
    )
    evalResult.values.toEither match {
      case Left(msg) => throw new Exception(s"format $task failed: $msg")
      case Right(_)  => ()
    }
  }

  private def invokeElmFormat(
      executable: os.Path,
      baseArgs: Seq[String],
      files: Seq[os.Path],
      checkMode: Boolean
  )(using TaskCtx): Unit = {
    val mode = if checkMode then Seq("--validate") else Seq("--yes")
    // Bypass Mill PathAliasing: java.io.File canonical paths are real filesystem strings.
    val fileArgs = files.map(p => p.toNIO.toFile.getCanonicalPath)
    val args     = mode ++ Seq("--elm-version=0.19") ++ fileArgs
    Task.log.info(
      s"format: ${if checkMode then "checking" else "formatting"} ${files.size} Elm path(s) via elm-format"
    )
    val exeStr = absoluteExecutable(executable)
    val argv   = exeStr +: (baseArgs ++ args)
    val result =
      os.proc(argv)
        .call(
          cwd = os.Path(BuildCtx.workspaceRoot.toNIO.toFile.getCanonicalPath),
          stdout = os.Pipe,
          stderr = os.Pipe,
          check = false
        )
    if result.exitCode != 0 then
      val err    = result.err.text().trim
      val out    = result.out.text().trim
      val detail =
        if err.nonEmpty then err
        else if out.nonEmpty then out
        else s"elm-format exited ${result.exitCode}"
      throw new Exception(s"format elm-format failed: $detail")
  }

  /**
   * Mill PathAliasing turns workspace paths into `../mill-workspace/...` strings. Build a real absolute path string via
   * `java.io.File` so `os.proc` can exec elm-format.
   */
  private def absoluteExecutable(path: os.Path): String = {
    val asString       = path.toString
    val marker         = "mill-workspace/"
    val idx            = asString.indexOf(marker)
    val workspaceCanon = BuildCtx.workspaceRoot.toNIO.toFile.getCanonicalPath
    if idx >= 0 then
      val relative = asString.substring(idx + marker.length)
      new java.io.File(workspaceCanon, relative).getCanonicalPath
    else path.toNIO.toFile.getCanonicalPath
  }

  private def scalafmtConfigPathRef(): PathRef = {
    val candidates = Seq(
      BuildCtx.workspaceRoot / ".scalafmt.conf",
      os.pwd / ".scalafmt.conf"
    )
    candidates.find(os.exists) match {
      case Some(path) => PathRef(path)
      case None       =>
        throw new Exception(
          s"format: no .scalafmt.conf found (searched ${candidates.mkString(", ")})"
        )
    }
  }

  private def toWorkspaceRelPath(workspace: os.Path, raw: String): os.RelPath = {
    val path = os.Path(raw, workspace)
    path.relativeTo(workspace)
  }

  private def expandScalaFiles(workspace: os.Path, rels: Seq[os.RelPath]): Seq[os.Path] =
    expandByExtension(workspace, rels, Set("scala", "mill"))

  private def expandElmFiles(workspace: os.Path, rels: Seq[os.RelPath]): Seq[os.Path] =
    expandByExtension(workspace, rels, Set("elm"))

  private def expandByExtension(
      workspace: os.Path,
      rels: Seq[os.RelPath],
      extensions: Set[String]
  ): Seq[os.Path] =
    rels.flatMap { rel =>
      val abs = workspace / rel
      if !os.exists(abs) then Seq.empty
      else if os.isDir(abs) then
        os.walk(
          abs,
          skip = p => p.last == "elm-stuff" || p.last == "node_modules" || p.last == "out"
        ).filter(p => os.isFile(p) && extensions.contains(p.ext))
      else if extensions.contains(abs.ext) then Seq(abs)
      else Seq.empty
    }.distinct

  private def discoverElmSourceFiles(workspace: os.Path): Seq[os.Path] = {
    val roots = Seq(
      workspace / "examples" / "morphir-elm-projects",
      workspace / "morphir-elm"
    )
    roots
      .filter(os.isDir)
      .flatMap { root =>
        os.walk(
          root,
          skip = p => p.last == "elm-stuff" || p.last == "node_modules" || p.last == "out"
        ).filter(p => os.isFile(p) && p.ext == "elm")
      }
      .distinct
      .sorted
  }

  private def unwrap[T](result: Result[T], what: String): T =
    result.toEither match {
      case Right(value) => value
      case Left(msg)    => throw new Exception(s"$what failed: $msg")
    }
}
