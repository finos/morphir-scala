package millbuild

/**
 * Pure path selection for the Mill format entrypoint and shared `ci.lint` checks.
 *
 * Routes filesystem paths by extension, filters git `--changed` candidates by [[FormatKind]], parses
 * `git status --porcelain`, and discovers the explicit Mill build-file set that full scala formatting / lint must cover
 * beyond `__.sources`.
 */
object FormatSelection {

  /** Paths split by which formatter owns them. */
  final case class RoutedPaths(
      scalaPaths: Seq[os.RelPath],
      elmPaths: Seq[os.RelPath],
      ignored: Seq[os.RelPath]
  )

  /** Roots under the workspace that contribute `*.mill` build files to the full scala sweep. */
  val buildMillRoots: Seq[os.RelPath] =
    Seq(os.RelPath("ci"), os.RelPath("mill-plugins"), os.RelPath("mill-build"))

  def scalaExtensions(kind: FormatKind): Boolean =
    kind match {
      case FormatKind.All | FormatKind.Scala => true
      case FormatKind.Elm                    => false
    }

  def elmExtensions(kind: FormatKind): Boolean =
    kind match {
      case FormatKind.All | FormatKind.Elm => true
      case FormatKind.Scala                => false
    }

  def isScalaPath(path: os.RelPath): Boolean = {
    val ext = path.ext
    ext == "scala" || ext == "mill"
  }

  def isElmPath(path: os.RelPath): Boolean =
    path.ext == "elm"

  /** Split paths into scalafmt (`.scala` / `.mill`), elm-format (`.elm`), and everything else. */
  def routeByExtension(paths: Seq[os.RelPath]): RoutedPaths = {
    val scalaBuf   = Seq.newBuilder[os.RelPath]
    val elmBuf     = Seq.newBuilder[os.RelPath]
    val ignoredBuf = Seq.newBuilder[os.RelPath]
    paths.foreach { path =>
      if isScalaPath(path) then scalaBuf += path
      else if isElmPath(path) then elmBuf += path
      else ignoredBuf += path
    }
    RoutedPaths(scalaBuf.result(), elmBuf.result(), ignoredBuf.result())
  }

  /**
   * Keep paths whose extensions belong to `kind`. Non-format paths (e.g. `.md`) are dropped for every kind — including
   * [[FormatKind.All]].
   */
  def filterChanged(paths: Seq[os.RelPath], kind: FormatKind): Seq[os.RelPath] =
    paths.filter { path =>
      (isScalaPath(path) && scalaExtensions(kind)) || (isElmPath(path) && elmExtensions(kind))
    }

  /**
   * Parse `git status --porcelain` into workspace-relative paths suitable for `--changed`.
   *
   * Includes modified, staged, added, renamed (destination), copied (destination), and untracked paths. Skips pure
   * deletes and ignored entries. Does not invoke git — callers supply the text.
   */
  def gitStatusPaths(porcelain: String): Seq[os.RelPath] =
    porcelain
      .linesIterator
      .map(_.stripTrailing())
      .filter(_.nonEmpty)
      .flatMap(parsePorcelainLine)
      .toSeq
      .distinct

  /**
   * Discover `*.mill` build files for the full scala format / lint surface: root `build.mill` plus every `*.mill` under
   * [[buildMillRoots]] (`ci/`, `mill-plugins/`, `mill-build/`).
   */
  def discoverBuildMillFiles(workspace: os.Path): Seq[os.RelPath] = {
    val rootBuild = workspace / "build.mill"
    val fromRoot  =
      if os.isFile(rootBuild) then Seq(os.RelPath("build.mill")) else Seq.empty

    val fromRoots = buildMillRoots.flatMap { root =>
      val abs = workspace / root
      if !os.isDir(abs) then Seq.empty
      else
        os.walk(abs)
          .filter(p => os.isFile(p) && p.ext == "mill")
          .map(_.relativeTo(workspace))
    }

    (fromRoot ++ fromRoots).distinct.sorted
  }

  private def parsePorcelainLine(line: String): Option[os.RelPath] = {
    if line.length < 3 then return None
    val xy = line.substring(0, 2)
    if !isChangedStatus(xy) then return None
    val rest    = line.substring(3)
    val rawPath =
      if rest.contains(" -> ") then rest.substring(rest.lastIndexOf(" -> ") + 4)
      else rest
    Some(os.RelPath(unquoteGitPath(rawPath.trim)))
  }

  /**
   * True when porcelain XY indicates a path we may want to format: modified, added, renamed, copied, unmerged, or
   * untracked — but not a pure delete or ignored entry.
   */
  private def isChangedStatus(xy: String): Boolean =
    if xy == "!!" then false
    else if isPureDelete(xy) then false
    else
      xy.exists { c =>
        c == 'M' || c == 'A' || c == 'R' || c == 'C' || c == 'U' || c == '?'
      }

  private def isPureDelete(xy: String): Boolean =
    (xy(0) == 'D' || xy(0) == ' ') && (xy(1) == 'D' || xy(1) == ' ') && xy.contains('D')

  private def unquoteGitPath(path: String): String =
    if path.length >= 2 && path.startsWith("\"") && path.endsWith("\"") then
      // Git C-style quotes: unescape common sequences; enough for format path selection.
      path
        .substring(1, path.length - 1)
        .replace("\\\\", "\\")
        .replace("\\\"", "\"")
        .replace("\\n", "\n")
        .replace("\\t", "\t")
    else path
}
