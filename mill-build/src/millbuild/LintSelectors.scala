package millbuild

/**
 * Pure selection over resolved `*.sources` module paths for [[build.ci.MorphirCiModule.lint]].
 *
 * `ci/MorphirCi.mill` supplies the names Mill resolved; this object drops those whose path matches `--exclude`. The
 * selector is `morphir.__.sources` so modules without `ScalafmtModule` stay in the check set; `checkFormatAll` lints
 * those files.
 */
object LintSelectors {
  val sourcesSelector: String = "morphir.__.sources"
  val sourcesSuffix: String   = ".sources"

  def modulesFromResolved(resolved: Seq[String]): Seq[String] =
    resolved
      .filter(_.endsWith(sourcesSuffix))
      .map { rendered =>
        if rendered.contains(' ') then
          throw new IllegalArgumentException(s"resolve $sourcesSelector: unexpected space in $rendered")
        rendered.stripSuffix(sourcesSuffix)
      }
      .distinct
      .sorted

  /**
   * Drop every module path that contains a match for `exclude`.
   *
   * A blank `exclude` keeps `modules` unchanged. An empty regex would otherwise match every path, so it is treated as
   * "no filter" rather than compiled.
   */
  def excludeMatching(modules: Seq[String], exclude: String): Either[String, Seq[String]] =
    if exclude.isBlank then Right(modules)
    else
      try
        val pattern = exclude.r
        Right(modules.filterNot(module => pattern.findFirstIn(module).nonEmpty))
      catch
        case error: java.util.regex.PatternSyntaxException =>
          Left(s"ci.lint --exclude is not a valid regex: ${error.getDescription}")
}
