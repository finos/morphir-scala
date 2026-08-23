package morphir.langkit.markdown

import kyo.*
import kyo.test.*

/**
 * Fixture scaffolding shared by the two suites that measure against the vendored conformance suites:
 * [[ConformanceTests]], which scores the parse-and-compile path, and [[WriterFidelityTests]], which measures the
 * writer's round trip over the same fixtures. Both read the same fixture files under the same shape, so the case
 * classes, the loading code and the profile mapping live here once rather than as two copies that could drift.
 */
private[markdown] object ConformanceFixtures:

  /** The file both suites read a profile's recorded conformance from. */
  val BaselinesFile = "conformance-baselines.json"

  /**
   * Absolute directory holding the fixture files, supplied by the build.
   *
   * Produced by the `markdownConformanceFixtures` task in `testing.mill`: the CommonMark fixture and the conformance
   * baselines are vendored and copied through unchanged, and the GFM fixture is derived from GitHub's specification
   * text, since GitHub publishes no JSON form of its own. `MorphirMarkdownConformanceEnv` (JVM and Native) and
   * `MorphirMarkdownConformanceJsEnv` (JS) — both also in `testing.mill` — pass that task's output directory to this
   * suite through `MORPHIR_CONFORMANCE_FIXTURES`.
   *
   * Resolved through [[kyo.Flag]] because it is the one env-and-property reader that works the same on all three
   * platforms, reading Node's `process.env` under Scala.js rather than `System.getenv`, which always returns null
   * there. The property form is for anyone running these suites outside Mill.
   */
  val fixturesDir: String =
    val configured = Flag[String]("morphir.conformance.fixtures", "")
    if configured.nonEmpty then configured
    else
      throw new IllegalStateException(
        "morphir.conformance.fixtures is unset, so the harness cannot find its fixtures. Mill sets it from " +
          "MorphirMarkdownConformanceEnv (JVM and Native) and MorphirMarkdownConformanceJsEnv (JS); outside Mill, " +
          "pass the absolute path to a directory holding commonmark-0.31.2-spec.json, gfm-0.29-spec.json and " +
          "conformance-baselines.json — the same three files markdownConformanceFixtures produces."
      )

  /**
   * One example a profile records rather than targets, with why.
   *
   * A conformance suite can be wrong about its own dialect. Deleting the example would hide that; counting it as a
   * failure forever says we have work left that we do not. Recording it keeps the example, subtracts it from the
   * measured set, and makes the reason travel with the score.
   */
  final case class Divergence(example: Int, reason: String) derives Schema

  /**
   * One profile's recorded conformance.
   *
   * `total` is not derived from the fixture file on purpose. Holding it here is what turns "we pass 652" into "we pass
   * 652 of the 652 in CommonMark 0.31.2", and what makes vendoring a different suite a deliberate act rather than an
   * accident that silently moves the goalposts.
   */
  final case class Baseline(
      profile: String,
      version: String,
      fixtures: String,
      passing: Int,
      total: Int,
      divergences: Chunk[Divergence] = Chunk.empty
  ) derives Schema:
    def name: String = s"$profile $version"

    /** Example numbers this profile does not target. */
    def divergedExamples: Set[Int] = divergences.map(_.example).toSet

  /**
   * The part of a fixture entry either suite reads. The published JSON also carries `start_line` and `end_line`, which
   * decoding ignores.
   *
   * `extension` is present only on an example whose own fence names one — GitHub's spec runner enables exactly the
   * extension an example's own fence claims, not the full GFM profile, and `profileOf(example: Example)` below mirrors
   * that. A CommonMark fixture entry carries no `extension` key at all, and decodes the same way: absent.
   */
  final case class Example(
      markdown: String,
      html: String,
      example: Int,
      section: String,
      extension: Maybe[String] = Absent
  ) derives Schema

  /**
   * Reads one fixture file eagerly, outside the effect system.
   *
   * `Path#read` is `Sync & Abort[FileReadException]`, but a baseline suite needs its fixtures while the test tree is
   * being built — `baselines.foreach` in `ConformanceTests` declares one pair of tests per profile — and a test tree
   * cannot be constructed inside a `Sync`. The unsafe view is the honest way to say that: this is a test harness
   * reading a file it vendors itself, at class-initialization time, and a failure should abort the suite rather than be
   * threaded anywhere.
   */
  def readFixture(name: String): String =
    import AllowUnsafe.embrace.danger
    val file = (Path(fixturesDir) / name).unsafe
    file.read() match
      case Result.Success(text) => text
      case other => throw new IllegalStateException(s"missing or unreadable fixture ${file.show}: $other")

  def decode[A](fixture: String)(using Schema[A], reflect.ClassTag[A]): A =
    Json.decode[A](readFixture(fixture)) match
      case Result.Success(parsed) => parsed
      case other                  => throw new IllegalStateException(s"could not read $fixture: $other")

  /**
   * The parse profile a recorded profile is measured under.
   *
   * A conformance suite measures a dialect, and a dialect here is an [[MdProfile]] — so the mapping has to be explicit
   * rather than defaulted, otherwise a GFM suite would be scored against a CommonMark parse and the extensions would
   * read as unimplemented forever. An unrecognised name fails loudly for the same reason: a typo in the baselines file
   * must not silently fall back to the base grammar.
   */
  def profileOf(baseline: Baseline): MdProfile = baseline.profile match
    case "CommonMark"               => MdProfile.commonmark
    case "GitHub Flavored Markdown" => MdProfile.gfm
    case other                      =>
      throw new IllegalStateException(
        s"$BaselinesFile names profile '$other', which this harness cannot map to an MdProfile. " +
          "Add it to profileOf alongside the profile's fixtures."
      )

  /**
   * The parse profile one example is measured under.
   *
   * A conformance suite for a dialect of extensions measures each extension against the base grammar, not against every
   * other extension turned on at once. cmark-gfm's own spec runner enables exactly the one extension an example's fence
   * names — an example fenced ` ```` example table ```` ` is a claim about tables and says nothing about strikethrough,
   * autolinks or the tag filter — so scoring it under every extension at once asks an example that makes no claim
   * about, say, the tag filter to behave as though GitHub's own runner had turned the tag filter on for it anyway. That
   * is exactly backwards, and it is what made five `HTML blocks` examples holding `<script>`/`<style>` read as
   * tag-filter failures even though neither their fence nor the published fixture ever claims the tag filter for them:
   * their own extension field is absent, so [[MdExtension.TagFilter]] must be off when they are measured, regardless of
   * which baseline's fixture they came from.
   *
   * `disabled` stands for [[MdExtension.TaskListItems]] rather than being its own extension: the two examples carrying
   * it are both in the *Task list items (extension)* section, and cmark-gfm's own runner marks them `disabled` because
   * its rendering of the checkbox input differs from what the prose shows — the same reason [[MdExtension.specTag]]
   * documents for why the tag stays `tasklist` there instead of splitting into a second name.
   */
  def profileOf(example: Example): MdProfile = example.extension match
    case Absent              => MdProfile.commonmark
    case Present("disabled") => MdProfile.commonmark.withExtension(MdExtension.TaskListItems)
    case Present(tag)        =>
      MdExtension.values.find(_.specTag == tag) match
        case Some(extension) => MdProfile.commonmark.withExtension(extension)
        case None            =>
          throw new IllegalStateException(
            s"example ${example.example} in section '${example.section}' names extension '$tag', which this " +
              "harness cannot map to an MdExtension. Add it to profileOf(example: Example) alongside the extension " +
              "it identifies."
          )
