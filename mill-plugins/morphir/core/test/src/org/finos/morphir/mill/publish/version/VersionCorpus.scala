package org.finos.morphir.mill.publish.version

/**
 * Reads `.config/version-rules` into typed cases.
 *
 * The corpus is shared with `.claude/skills/squire`, which has its own loader against the same files. Neither loader is
 * the owner: the JSON is, and a field renamed here has to be renamed there too, which is exactly the coupling the
 * corpus is for.
 */
object VersionCorpus {
  final case class ParseCase(text: String, major: Int, minor: Int, patch: Int, prerelease: Option[String])
  final case class RejectCase(text: String, why: String)
  final case class CompareCase(left: String, right: String, sign: Int)
  final case class CompareRejectCase(left: String, right: String, messageContains: String)
  final case class StreamCase(
      namespace: Option[String],
      pattern: String,
      tagFor: Map[String, String],
      versionFromTag: Map[String, Option[String]]
  )
  final case class ReleaseLineCase(file: String, text: String, source: String, releaseLine: String)
  final case class ReleaseLineRejectCase(file: String, text: String, source: String, message: String)
  final case class HeadingRow(version: String, date: Option[String])
  final case class HeadingCase(file: String, text: String, headings: List[HeadingRow])
  final case class AreaCase(
      name: String,
      namespace: Option[String],
      changelogPath: String,
      startingVersion: Option[String],
      millSelector: String
  )

  final case class Corpus(
      parseCases: List[ParseCase],
      rejectCases: List[RejectCase],
      compareCases: List[CompareCase],
      compareRejectCases: List[CompareRejectCase],
      streamCases: List[StreamCase],
      releaseLineCases: List[ReleaseLineCase],
      releaseLineRejectCases: List[ReleaseLineRejectCase],
      headingCases: List[HeadingCase],
      areaCases: List[AreaCase]
  )

  /**
   * `MORPHIR_VERSION_RULES_DIR` is set by the test module, which knows the workspace root. The upward search is the
   * fallback for running this suite outside Mill, and fails loudly rather than silently testing nothing.
   */
  def directory(): os.Path =
    // Resolved against the working directory, since Mill hands the forked test a path relative to its sandbox.
    sys.env.get("MORPHIR_VERSION_RULES_DIR").filter(_.nonEmpty).map(os.Path(_, os.pwd)) match {
      case Some(path) if os.exists(path) => path
      case _                             =>
        val start = os.pwd
        Iterator
          .iterate(start)(_ / os.up)
          .takeWhile(path => path != path / os.up)
          .map(_ / ".config" / "version-rules")
          .find(os.exists)
          .getOrElse(
            sys.error(s"no .config/version-rules found from $start; set MORPHIR_VERSION_RULES_DIR")
          )
    }

  def load(root: os.Path = directory()): Corpus = {
    val json      = ujson.read(os.read(root / "corpus.json"))
    val changelog = (file: String) => os.read(root / "changelogs" / file)

    def optional(value: ujson.Value): Option[String] = if (value.isNull) None else Some(value.str)

    Corpus(
      parseCases = json("semverParse").arr.map { row =>
        ParseCase(
          row("text").str,
          row("major").num.toInt,
          row("minor").num.toInt,
          row("patch").num.toInt,
          optional(row("prerelease"))
        )
      }.toList,
      rejectCases = json("semverRejects").arr.map(row => RejectCase(row("text").str, row("why").str)).toList,
      compareCases = json("semverCompare").arr.map { row =>
        CompareCase(row("left").str, row("right").str, row("sign").num.toInt)
      }.toList,
      compareRejectCases = json("semverCompareRejects").arr.map { row =>
        CompareRejectCase(row("left").str, row("right").str, row("messageContains").str)
      }.toList,
      streamCases = json("tagStreams").arr.map { row =>
        StreamCase(
          optional(row("namespace")),
          row("pattern").str,
          row("tagFor").arr.map(pair => pair("version").str -> pair("tag").str).toMap,
          row("versionFromTag").arr.map(pair => pair("tag").str -> optional(pair("version"))).toMap
        )
      }.toList,
      releaseLineCases = json("changelogReleaseLine").arr.map { row =>
        val file = row("file").str
        ReleaseLineCase(file, changelog(file), row("source").str, row("releaseLine").str)
      }.toList,
      releaseLineRejectCases = json("changelogReleaseLineRejects").arr.map { row =>
        val file = row("file").str
        ReleaseLineRejectCase(file, changelog(file), row("source").str, row("message").str)
      }.toList,
      headingCases = json("changelogHeadings").arr.map { row =>
        val file = row("file").str
        HeadingCase(
          file,
          changelog(file),
          row("headings").arr.map(heading => HeadingRow(heading("version").str, optional(heading("date")))).toList
        )
      }.toList,
      areaCases = json("areas").arr.map { row =>
        AreaCase(
          row("name").str,
          optional(row("namespace")),
          row("changelogPath").str,
          optional(row("startingVersion")),
          row("millSelector").str
        )
      }.toList
    )
  }
}
