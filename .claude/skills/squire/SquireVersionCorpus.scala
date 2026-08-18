//| scalaVersion: 3.8.4
//| moduleDeps: [SquireModel.scala]

import java.nio.file.Files
import kyo.*

/**
 * Reads `.config/version-rules` into typed cases, for asserting the [[SquireVersion]] port against the same data the
 * originals in `mill-plugins/morphir/core` are asserted against.
 *
 * There is a second loader over these files, `VersionCorpus` in that module's test sources. Neither owns the shape:
 * the JSON does, and a field renamed on one side has to be renamed on the other. That is the coupling the corpus is
 * for, made visible rather than left to a doc comment.
 *
 * The record fields are named to match the JSON keys exactly, since the decode is by field name.
 */
object SquireVersionCorpus:

  final case class ParseCase(text: String, major: Int, minor: Int, patch: Int, prerelease: Maybe[String]) derives Schema
  final case class RejectCase(text: String, why: String) derives Schema
  final case class CompareCase(left: String, right: String, sign: Int) derives Schema
  final case class CompareRejectCase(left: String, right: String, messageContains: String) derives Schema
  final case class TagForCase(version: String, tag: String) derives Schema
  final case class VersionFromTagCase(tag: String, version: Maybe[String]) derives Schema
  final case class StreamCase(
      namespace: Maybe[String],
      pattern: String,
      tagFor: List[TagForCase],
      versionFromTag: List[VersionFromTagCase]
  ) derives Schema
  final case class ReleaseLineCase(file: String, source: String, releaseLine: String) derives Schema
  final case class ReleaseLineRejectCase(file: String, source: String, message: String) derives Schema
  final case class HeadingRow(version: String, date: Maybe[String]) derives Schema
  final case class HeadingCase(file: String, headings: List[HeadingRow]) derives Schema
  final case class AreaCase(
      name: String,
      namespace: Maybe[String],
      changelogPath: String,
      startingVersion: Maybe[String],
      millSelector: String
  ) derives Schema

  final case class Corpus(
      semverParse: List[ParseCase],
      semverRejects: List[RejectCase],
      semverCompare: List[CompareCase],
      semverCompareRejects: List[CompareRejectCase],
      tagStreams: List[StreamCase],
      changelogReleaseLine: List[ReleaseLineCase],
      changelogReleaseLineRejects: List[ReleaseLineRejectCase],
      changelogHeadings: List[HeadingCase],
      areas: List[AreaCase]
  ) derives Schema

  /** Loads the corpus rooted at the given directory, and reads a changelog sample by the file name a case names. */
  def load(directory: Path): Corpus < (Sync & Abort[SquireError]) =
    Sync.defer(Files.readString((directory / "corpus.json").toJava)).map { text =>
      SquireJson.decode[Corpus](text) match
        case Result.Success(corpus) => corpus
        case failure                =>
          Abort.fail(
            SquireError.Failure("version-corpus", s"${directory / "corpus.json"} did not decode: $failure")
          )
    }

  /** The build's own view of an area, as `./mill show ci.releaseAreas` reports it. */
  final case class BuildReleaseArea(
      name: String,
      namespace: Maybe[String],
      changelogPath: String,
      startingVersion: Maybe[String]
  ) derives Schema

  def changelog(directory: Path, file: String): String < Sync =
    Sync.defer(Files.readString((directory / "changelogs" / file).toJava))
