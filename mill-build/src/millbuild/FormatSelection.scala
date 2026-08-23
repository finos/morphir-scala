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

  /**
   * Directory names skipped when walking the workspace for `*.mill` build files. Generated caches, VCS, and tool
   * scratch trees never contribute tracked format targets.
   */
  val buildMillSkipDirNames: Set[String] = Set(
    "out",
    ".git",
    "node_modules",
    "elm-stuff",
    ".bloop",
    ".metals",
    ".idea",
    "target",
    ".dev",
    ".ref",
    "ref"
  )

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
   * Keep paths whose extensions belong to `kind`, plus extensionless paths (directory arguments / bare names).
   *
   * Callers must expand directories after this filter and before [[routeByExtension]]; otherwise a directory such as
   * `examples/.../src` would be dropped here and never reach `expandScalaFiles` / `expandElmFiles`. Non-format files
   * (e.g. `.md`) are dropped for every kind — including [[FormatKind.All]].
   */
  def filterChanged(paths: Seq[os.RelPath], kind: FormatKind): Seq[os.RelPath] =
    paths.filter { path =>
      if path.ext.isEmpty then true
      else
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
   * Discover every `*.mill` build file under the workspace for the full scala format / lint surface, skipping
   * generated and tool-cache directories listed in [[buildMillSkipDirNames]] (and other hidden directories).
   */
  def discoverBuildMillFiles(workspace: os.Path): Seq[os.RelPath] = {
    if !os.isDir(workspace) then Seq.empty
    else
      os.walk(
        workspace,
        skip = p =>
          os.isDir(p) && (buildMillSkipDirNames.contains(p.last) || p.last.startsWith("."))
      ).filter(p => os.isFile(p) && p.ext == "mill")
        .map(_.relativeTo(workspace))
        .distinct
        .sorted
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
