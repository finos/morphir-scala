package millbuild

/**
 * Pure selection over resolved `*.checkFormat` module paths for [[build.ci.MorphirCiModule.lint]].
 *
 * `ci/MorphirCi.mill` supplies the names Mill resolved; this object drops those whose path matches
 * `--exclude`.
 */
object LintSelectors {
  val checkFormatSelector: String = "morphir.__.checkFormat"
  val checkFormatSuffix: String   = ".checkFormat"

  def modulesFromResolved(resolved: Seq[String]): Seq[String] =
    resolved
      .filter(_.endsWith(checkFormatSuffix))
      .map { rendered =>
        if rendered.contains(' ') then
          throw new IllegalArgumentException(s"resolve $checkFormatSelector: unexpected space in $rendered")
        rendered.stripSuffix(checkFormatSuffix)
      }
      .distinct
      .sorted

  /**
   * Drop every module path that contains a match for `exclude`.
   *
   * A blank `exclude` keeps `modules` unchanged. An empty regex would otherwise match every
   * path, so it is treated as "no filter" rather than compiled.
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
