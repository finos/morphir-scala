package morphir.langkit.markdown

/**
 * A kind of frontmatter block a profile may recognize, carrying its own delimiter.
 *
 * A parameterized enum is Scala 3's spelling of "cases extending an abstract class with a value": recognition is
 * data-driven from `delimiter` alone, with no per-kind parsing code.
 */
enum FrontMatterKind(val delimiter: String) derives CanEqual:
  /** A `---`-delimited YAML block. */
  case Yaml extends FrontMatterKind("---")
  // case Toml extends FrontMatterKind("+++")   — later

/**
 * What a parse recognizes beyond plain CommonMark.
 *
 * CommonMark itself has no frontmatter, so recognition is opt-in: the empty set is off, and the parser walks whichever
 * kinds are enabled, matching each one's `delimiter` as the exact opening and closing line. Several kinds may be
 * enabled at once; no separate on/off/auto mode is needed because the set already carries that.
 */
final case class MdProfile(frontmatter: Set[FrontMatterKind] = Set.empty) derives CanEqual:
  /** This profile, with YAML frontmatter recognition added. */
  def withYamlFrontmatter: MdProfile = copy(frontmatter = frontmatter + FrontMatterKind.Yaml)

  /** Whether any frontmatter kind is recognized at all — the empty set is off. */
  inline def supportsFrontMatter: Boolean = frontmatter.nonEmpty

object MdProfile:
  /** Plain CommonMark: no frontmatter kind recognized. Conformance suites stay on this by construction. */
  val commonmark: MdProfile = MdProfile()

  given default: MdProfile = commonmark
