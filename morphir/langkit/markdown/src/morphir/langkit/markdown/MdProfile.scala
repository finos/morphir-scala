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
 * CommonMark itself has neither frontmatter nor any of GFM's additions, so both are opt-in and both are held as a set
 * of what is on. The empty set is off; the parser walks whichever members are enabled. Two sets rather than one because
 * they answer different questions — a frontmatter kind is a delimiter to recognize at document start, a [[MdExtension]]
 * is a grammar rule to switch on — and because GitHub's own dialect enables the second without the first.
 *
 * Per-extension configuration is deliberately absent. Every value it would hold today — the tag filter's tag list, the
 * autolink scheme set, the strikethrough tilde rule — is pinned by the GFM specification, so each key would ship with
 * exactly one conformant setting. When a consumer needs it, it arrives here as a third defaulted field, an
 * [[MdMeta]]-shaped `options: Map[MdOptionKey[?], Any]`, and not as state on [[MdExtension]]: an enum case is a
 * singleton shared by every profile, so a map on it would be shared too, and parameterizing the cases would cost set
 * membership and with it [[supports]]. Writer-side spellings belong to [[MdStyle]] and go there.
 */
final case class MdProfile(
    frontmatter: Set[FrontMatterKind] = Set.empty,
    extensions: Set[MdExtension] = Set.empty
) derives CanEqual:
  /** This profile, with YAML frontmatter recognition added. */
  def withYamlFrontmatter: MdProfile = copy(frontmatter = frontmatter + FrontMatterKind.Yaml)

  /** This profile, with one more extension enabled. */
  def withExtension(extension: MdExtension): MdProfile = copy(extensions = extensions + extension)

  /** This profile, with every extension in `added` enabled alongside whatever it already had. */
  def withExtensions(added: Set[MdExtension]): MdProfile = copy(extensions = extensions ++ added)

  /** Whether any frontmatter kind is recognized at all — the empty set is off. */
  inline def supportsFrontMatter: Boolean = frontmatter.nonEmpty

  /** Whether one extension is enabled. */
  inline def supports(extension: MdExtension): Boolean = extensions.contains(extension)

object MdProfile:
  /** Plain CommonMark: nothing recognized beyond the base grammar. */
  val commonmark: MdProfile = MdProfile()

  /**
   * GitHub Flavored Markdown: CommonMark plus all five extensions, and no frontmatter.
   *
   * Frontmatter is left off because the GFM specification has none — GitHub strips a YAML block in some surfaces and
   * renders it in others, and neither behaviour is what the conformance fixtures measure. A caller wanting both writes
   * `MdProfile.gfm.withYamlFrontmatter`.
   */
  val gfm: MdProfile = commonmark.withExtensions(MdExtension.gfm)

  given default: MdProfile = commonmark
