//| scalaVersion: 3.8.4
//| moduleDeps: [SquireModel.scala, SquireProcess.scala]

import kyo.*

/**
 * The changelog and release-tag rules the build enforces, ported here so squire can check and act on them
 * without asking a contributor to run the build.
 *
 * This mirrors — does not import — `SemVer`, `Changelog` and `TagStream` from
 * `mill-plugins/morphir/core/src/org/finos/morphir/mill/publish/version/`. Squire cannot moduleDep on that
 * module: its Mill workspace root is `.claude/skills/squire/`, single-file scripts can only moduleDep on
 * other scripts within that same workspace (a relative path that climbs out of it, `../../../mill-plugins/...`,
 * fails immediately with `requirement failed: ups must be zero`, and a `//workspace-relative` path is resolved
 * from that same root, so it cannot reach outside either), and the target files are not scripts in the first
 * place — they are ordinary module sources compiled as part of `mill-plugins.morphir.core` through the root
 * repository's metabuild, which a standalone script has no way to bootstrap. Depending on a published
 * `org.finos.morphir.mill:mill-morphir-core` coordinate was the other option; it was rejected because during
 * development the coordinate this branch would need is not yet published (the usual chicken-and-egg for a
 * dev-environment tool that must also work offline), and because it would tie squire's own build to network
 * access and a version pin for three small, stable, already-tested functions.
 *
 * Keep any rule change mirrored in both places. `ChangelogTests.scala`, `SemVerTests.scala` and
 * `TagStreamTests.scala` under `mill-plugins/morphir/core` stay the source of truth for the rules themselves;
 * this file must keep matching them.
 *
 * That instruction is now enforced rather than merely stated. `.config/version-rules/corpus.json` holds the
 * inputs and expected outputs both implementations must satisfy: `VersionCorpusTests` asserts the originals
 * against it, and `SquireChangelogSpec` asserts this port against it. A rule changed on one side alone fails a
 * test on the other. Add a case to the corpus rather than to one suite.
 */
object SquireVersion:
  final case class SemVer(major: Int, minor: Int, patch: Int, prerelease: Option[String])

  object SemVer:
    private val Pattern = raw"^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(?:-([0-9A-Za-z.-]+))?$$".r

    def parse(text: String): Option[SemVer] = text match
      case Pattern(major, minor, patch, prerelease) if Option(prerelease).forall(validPrerelease) =>
        Some(SemVer(major.toInt, minor.toInt, patch.toInt, Option(prerelease)))
      case _ => None

    /** Rejects empty identifiers (`1.2.3-alpha..1`) and leading-zero numeric ones (`1.2.3-01`), which `Pattern` admits. */
    private def validPrerelease(prerelease: String): Boolean =
      prerelease.split("\\.", -1).forall { identifier =>
        identifier.nonEmpty &&
        (!identifier.forall(_.isDigit) || identifier == "0" || !identifier.startsWith("0"))
      }

    /** Orders two versions, negative when `a` precedes `b`. Mirrors `org.finos...version.SemVer.compare`. */
    def compare(a: String, b: String): Either[String, Int] =
      (parse(a), parse(b)) match
        case (None, _) => Left(s"'$a' is not a semantic version")
        case (_, None) => Left(s"'$b' is not a semantic version")
        case (Some(left), Some(right)) =>
          val numeric =
            Ordering[(Int, Int, Int)].compare((left.major, left.minor, left.patch), (right.major, right.minor, right.patch))
          if numeric != 0 then Right(numeric.sign)
          else
            Right((left.prerelease, right.prerelease) match
              case (None, None)             => 0
              case (None, Some(_))          => 1
              case (Some(_), None)          => -1
              case (Some(one), Some(other)) => one.compareTo(other).sign
            )

  final case class ChangelogHeading(version: String, date: Option[String])

  object Changelog:
    // `## [0.6.0-M01] - 2026-07-14`, `## [0.6.0-M01]`, `## 0.6.0-M01`, with or without brackets.
    private val Heading = raw"^##\s+\[?([^\]\s]+)\]?\s*(?:-\s*(\d{4}-\d{2}-\d{2}))?\s*$$".r

    private def candidates(text: String): Seq[ChangelogHeading] =
      text.linesIterator.collect { case Heading(version, date) => ChangelogHeading(version, Option(date)) }.toSeq

    /** Every version heading, in document order. Non-version headings such as Unreleased are dropped. */
    def headings(text: String): Seq[ChangelogHeading] =
      candidates(text).filter(heading => SemVer.parse(heading.version).isDefined)

    /** The topmost undated version heading. Mirrors `org.finos...version.Changelog.releaseLine` exactly. */
    def releaseLine(text: String, source: String): Either[String, String] =
      val undatedCandidates = candidates(text).filter(_.date.isEmpty).map(_.version).filter(_ != "Unreleased")
      val undatedVersions   = undatedCandidates.filter(candidate => SemVer.parse(candidate).isDefined)
      undatedVersions match
        case Seq(single) => Right(single)
        case Seq() =>
          undatedCandidates.headOption match
            case Some(notAVersion) => Left(s"$source: the undated heading '$notAVersion' is not a semantic version")
            case None =>
              Left(s"$source: no undated release heading. Add one naming the next release, for example '## [0.1.0]'")
        case many => Left(s"$source: expected one undated release heading, found ${many.mkString(", ")}")

    /** The exact line index of the topmost undated heading naming `version`, if one is present. */
    def undatedHeadingLine(lines: IndexedSeq[String], version: String): Option[Int] =
      lines.indexWhere {
        case Heading(headingVersion, date) => headingVersion == version && Option(date).isEmpty
        case _                             => false
      } match
        case -1    => None
        case index => Some(index)

  /**
   * The git tag stream an independently versioned area releases through. Mirrors
   * `org.finos...version.TagStream` exactly.
   */
  final case class TagStream(namespace: Option[String]):
    private def prefix: String = namespace.fold("v")(value => s"$value/v")
    def pattern: String        = s"$prefix*"
    def tagFor(version: String): String = s"$prefix$version"
    def versionFromTag(tag: String): Option[String] =
      if !tag.startsWith(prefix) then None
      else
        val candidate = tag.drop(prefix.length)
        if candidate.contains('/') then None else Some(candidate)

/** One independently versioned area: which changelog states its next release, and the floor that release must clear. */
final case class ReleaseArea(name: String, namespace: Option[String], changelogPath: String, floor: Option[String])

final case class AreaOutcome(
    area: String,
    changelogPath: String,
    status: String,
    releaseLine: Maybe[String] = Absent,
    detail: Maybe[String] = Absent
) derives Schema

final case class ChangelogReport(command: String, ok: Boolean, outcomes: List[AreaOutcome]) derives Schema

final case class AreaStatus(
    area: String,
    changelogPath: String,
    stream: String,
    status: String,
    releaseLine: Maybe[String] = Absent,
    tag: Maybe[String] = Absent,
    detail: Maybe[String] = Absent
) derives Schema

final case class ReleaseStatusReport(command: String, ok: Boolean, areas: List[AreaStatus]) derives Schema

final case class PrepareResult(
    area: String,
    changelogPath: String,
    version: String,
    date: String,
    tag: String,
    gitTagCommand: String,
    nextVersion: String
) derives Schema

trait ChangelogFileSystem:
  def exists(path: Path): Boolean < Sync
  def read(path: Path): String < Sync
  def write(path: Path, text: String): Unit < Sync

object LiveChangelogFileSystem extends ChangelogFileSystem:
  import java.nio.charset.StandardCharsets
  import java.nio.file.{Files, LinkOption, StandardOpenOption}

  def exists(path: Path): Boolean < Sync = path.exists
  def read(path: Path): String < Sync = Sync.defer {
    val input = Files.newInputStream(path.toJava, StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS)
    try new String(input.readAllBytes(), StandardCharsets.UTF_8)
    finally input.close()
  }
  def write(path: Path, text: String): Unit < Sync = Sync.defer {
    val output = Files.newOutputStream(
      path.toJava,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING,
      StandardOpenOption.WRITE,
      LinkOption.NOFOLLOW_LINKS
    )
    try output.write(text.getBytes(StandardCharsets.UTF_8))
    finally output.close()
  }

object SquireChangelog:
  import SquireVersion.*

  val Areas: List[ReleaseArea] = List(
    // Mirrors MorphirVersionedModule.startingVersion's default in build.mill: the libraries are the
    // one stream with real published history (~40 tags), so — unlike the two never-published areas
    // below, whose floors just sit above the shared tag line — this floor protects something.
    ReleaseArea("libraries", None, "CHANGELOG.md", Some("0.5.0-M04")),
    ReleaseArea("mill-plugins", Some("mill-plugins"), "mill-plugins/morphir/CHANGELOG.md", Some("0.5.0-M04")),
    ReleaseArea("desktop", Some("desktop"), "morphir/desktop/CHANGELOG.md", Some("0.1.0"))
  )

  private val IsoDate = raw"^\d{4}-\d{2}-\d{2}$$".r

  def findArea(name: String): Either[String, ReleaseArea] =
    Areas.find(_.name.equalsIgnoreCase(name)) match
      case Some(area) => Right(area)
      case None       => Left(s"unknown area '$name'; expected one of ${Areas.map(_.name).mkString(", ")}")

  /** Reads every area's changelog and enforces both rules the build enforces: one undated heading, at or above the floor. */
  def check(root: Path, files: ChangelogFileSystem = LiveChangelogFileSystem): ChangelogReport < Sync =
    Kyo.foreach(Areas)(area => checkArea(root, area, files)).map { outcomes =>
      ChangelogReport("changelog-check", outcomes.forall(_.status == "ok"), outcomes.toList)
    }

  /** Reads every area's changelog and reports its release line, without enforcing the floor. */
  def show(root: Path, files: ChangelogFileSystem = LiveChangelogFileSystem): ChangelogReport < Sync =
    Kyo.foreach(Areas)(area => releaseLineOutcome(root, area, files)).map { outcomes =>
      ChangelogReport("changelog-show", outcomes.forall(_.status == "ok"), outcomes.toList)
    }

  private def checkArea(root: Path, area: ReleaseArea, files: ChangelogFileSystem): AreaOutcome < Sync =
    releaseLineOutcome(root, area, files).map {
      case outcome @ AreaOutcome(_, _, "ok", Present(version), _) =>
        floorIssue(area, version) match
          case Some(message) => outcome.copy(status = "issue", detail = Present(message))
          case None           => outcome
      case outcome => outcome
    }

  private def releaseLineOutcome(root: Path, area: ReleaseArea, files: ChangelogFileSystem): AreaOutcome < Sync =
    val path = root / area.changelogPath
    files.exists(path).flatMap {
      case false => AreaOutcome(area.name, area.changelogPath, "issue", detail = Present(s"${area.changelogPath}: file does not exist"))
      case true =>
        files.read(path).map { text =>
          Changelog.releaseLine(text, area.changelogPath) match
            case Left(message)  => AreaOutcome(area.name, area.changelogPath, "issue", detail = Present(message))
            case Right(version) => AreaOutcome(area.name, area.changelogPath, "ok", releaseLine = Present(version))
        }
    }

  private def floorIssue(area: ReleaseArea, version: String): Option[String] =
    area.floor.flatMap { floor =>
      SemVer.compare(version, floor) match
        case Left(message)         => Some(s"${area.changelogPath}: $message")
        case Right(cmp) if cmp < 0 =>
          Some(s"${area.changelogPath}: release line $version is below the ${area.name} area's starting version $floor")
        case Right(_) => None
    }

  /**
   * Dates the topmost undated heading and inserts a fresh undated heading above it for the next cycle, so
   * `streamVersion`/`releaseLine` keeps resolving immediately afterwards instead of failing until a human
   * hand-writes the next number. The default next heading is the just-dated version with its patch component
   * incremented and its prerelease qualifier dropped; a person can always edit it before the next release.
   *
   * This is a post-release step, run *after* the tag it reports has already been pushed — not before. The
   * commit `prepare` reads must still carry the undated heading naming the version being released, because
   * that is what the release build matches against the tag; dating it here and only then re-committing would
   * make the release build compare the tag to whatever heading is undated *next*, which no longer agrees.
   * Refuses, writing nothing, unless the changelog has exactly one undated version heading and `date` is
   * `yyyy-MM-dd`.
   */
  def prepare(
      root: Path,
      areaName: String,
      date: String,
      files: ChangelogFileSystem = LiveChangelogFileSystem
  ): PrepareResult < (Sync & Abort[SquireError]) =
    findArea(areaName) match
      case Left(message) => Abort.fail(SquireError.Failure("release", message))
      case Right(area) =>
        if IsoDate.matches(date) then prepareArea(root, area, date, files)
        else Abort.fail(SquireError.Failure("release", s"date '$date' must be yyyy-MM-dd"))

  private def prepareArea(
      root: Path,
      area: ReleaseArea,
      date: String,
      files: ChangelogFileSystem
  ): PrepareResult < (Sync & Abort[SquireError]) =
    val path = root / area.changelogPath
    files.exists(path).flatMap {
      case false => Abort.fail(SquireError.Failure("release", s"${area.changelogPath}: file does not exist"))
      case true =>
        files.read(path).flatMap { text =>
          preparedText(text, area, date) match
            case Left(message) => Abort.fail(SquireError.Failure("release", message))
            case Right((version, nextVersion, updated)) =>
              files.write(path, updated).map { _ =>
                val tag = TagStream(area.namespace).tagFor(version)
                PrepareResult(area.name, area.changelogPath, version, date, tag, s"git tag $tag", nextVersion)
              }
        }
    }

  /** The default next undated heading: the patch component incremented, the prerelease qualifier dropped. */
  private def nextUndatedVersion(version: String): Either[String, String] =
    SemVer.parse(version) match
      case Some(SemVer(major, minor, patch, _)) => Right(s"$major.$minor.${patch + 1}")
      case None                                 => Left(s"'$version' is not a semantic version")

  private def preparedText(text: String, area: ReleaseArea, date: String): Either[String, (String, String, String)] =
    Changelog.releaseLine(text, area.changelogPath).flatMap { version =>
      val lines = text.split("\n", -1).toIndexedSeq
      Changelog.undatedHeadingLine(lines, version) match
        case None =>
          Left(s"${area.changelogPath}: could not find the undated heading line for $version to date")
        case Some(index) =>
          nextUndatedVersion(version).map { nextVersion =>
            val datedLine        = s"## [$version] - $date"
            val withDatedLine    = lines.updated(index, datedLine)
            val (before, after)  = withDatedLine.splitAt(index)
            val prepared         = before ++ IndexedSeq(s"## [$nextVersion]", "") ++ after
            (version, nextVersion, prepared.mkString("\n"))
          }
    }

  /** Reports, for every area, which stream a tag would release next and whether an existing tag on HEAD agrees with the changelog. */
  def status(
      root: Path,
      runner: ProcessRunner,
      files: ChangelogFileSystem = LiveChangelogFileSystem
  ): ReleaseStatusReport < (Async & Sync & Abort[SquireError]) =
    Kyo.foreach(Areas)(area => areaStatus(root, area, runner, files)).map { statuses =>
      ReleaseStatusReport("release-status", statuses.forall(_.status != "issue"), statuses.toList)
    }

  private def areaStatus(
      root: Path,
      area: ReleaseArea,
      runner: ProcessRunner,
      files: ChangelogFileSystem
  ): AreaStatus < (Async & Sync & Abort[SquireError]) =
    val stream = TagStream(area.namespace)
    val path   = root / area.changelogPath
    files.exists(path).flatMap {
      case false =>
        AreaStatus(area.name, area.changelogPath, stream.pattern, "issue", detail = Present(s"${area.changelogPath}: file does not exist"))
      case true =>
        files.read(path).flatMap { text =>
          Changelog.releaseLine(text, area.changelogPath) match
            case Left(message) =>
              AreaStatus(area.name, area.changelogPath, stream.pattern, "issue", detail = Present(message))
            case Right(version) =>
              headTagsForStream(root, stream, runner).map { headVersions =>
                headVersions.headOption match
                  case None =>
                    AreaStatus(
                      area.name,
                      area.changelogPath,
                      stream.pattern,
                      "pending",
                      releaseLine = Present(version),
                      tag = Present(stream.tagFor(version))
                    )
                  case Some(headVersion) =>
                    val headTag  = stream.tagFor(headVersion)
                    val recorded = Changelog.headings(text).exists(h => h.version == headVersion && h.date.isDefined)
                    if recorded then
                      AreaStatus(
                        area.name,
                        area.changelogPath,
                        stream.pattern,
                        "released",
                        releaseLine = Present(version),
                        tag = Present(headTag)
                      )
                    else
                      AreaStatus(
                        area.name,
                        area.changelogPath,
                        stream.pattern,
                        "issue",
                        releaseLine = Present(version),
                        tag = Present(headTag),
                        detail =
                          Present(s"HEAD is tagged $headTag but ${area.changelogPath} does not record $headVersion as a dated release")
                      )
              }
        }
    }

  /**
   * A stream with no tag at HEAD exits 0 with empty output, so an empty list is the ordinary "not released yet"
   * answer. A nonzero exit means the probe itself failed, for example outside a git worktree, and must not be
   * flattened into that answer: every area would then read `pending` and `release status` would exit 0 having
   * inspected nothing.
   */
  private def headTagsForStream(root: Path, stream: TagStream, runner: ProcessRunner): List[String] < (Async & Abort[SquireError]) =
    runner.run(ProcessRequest(Chunk("git", "tag", "--points-at", "HEAD", "--list", stream.pattern), Present(root))).map { result =>
      if result.exitCode != 0 then
        Abort.fail(
          SquireError.Failure(
            "release",
            s"git tag --points-at HEAD --list ${stream.pattern} failed (exit ${result.exitCode}): ${result.stderr.trim}"
          )
        )
      else result.stdout.linesIterator.map(_.trim).filter(_.nonEmpty).flatMap(stream.versionFromTag).toList
    }

  def renderChangelogReport(report: ChangelogReport): String =
    val rows = report.outcomes.map { outcome =>
      val mark = if outcome.status == "ok" then "  " else "❌"
      val body = outcome.releaseLine match
        case Present(version) => s"release line $version"
        case Absent           => outcome.detail.getOrElse("no release line")
      val extra = if outcome.status == "ok" then "" else outcome.detail.fold("")(detail => s" — $detail")
      f"$mark ${outcome.area}%-14s $body$extra"
    }
    val summary =
      if report.ok then s"${report.outcomes.size} area(s) OK"
      else s"${report.outcomes.count(_.status != "ok")} of ${report.outcomes.size} area(s) failing"
    (rows :+ "" :+ summary).mkString("\n") + "\n"

  def renderReleaseStatus(report: ReleaseStatusReport): String =
    val rows = report.areas.map { area =>
      val mark = if area.status == "issue" then "❌" else "  "
      val body = area.status match
        case "pending"  => s"pending — next tag ${area.tag.getOrElse("?")}"
        case "released" => s"released — HEAD is ${area.tag.getOrElse("?")}"
        case _          => area.detail.getOrElse("issue")
      f"$mark ${area.area}%-14s $body"
    }
    val summary = if report.ok then "release status: clean" else "release status: needs attention"
    (rows :+ "" :+ summary).mkString("\n") + "\n"

  def renderPrepare(result: PrepareResult): String =
    s"${result.changelogPath}: dated ${result.version} as ${result.date}\n" +
      s"(this runs after the release, not before — ${result.gitTagCommand} should already be pushed)\n" +
      s"next undated heading: ${result.nextVersion}\n"
