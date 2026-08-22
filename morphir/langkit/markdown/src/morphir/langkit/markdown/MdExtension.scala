package morphir.langkit.markdown

/**
 * A syntax extension a profile may recognize beyond the base CommonMark grammar.
 *
 * The five members are the ones the GitHub Flavored Markdown specification defines, and GFM marks every one of them
 * as an *extension* rather than folding it into the base grammar. Keeping that same seam here is what lets a profile
 * turn one on without the rest: a repository rendering release notes may want tables and strikethrough and nothing
 * else, and a document pipeline that sanitises its own HTML has no use for [[TagFilter]].
 *
 * The name is `MdExtension` rather than `GfmExtension` because the axis is wider than one dialect. GitHub's own
 * non-specified additions — footnotes, alerts, math — and any extension another dialect brings would join this enum
 * rather than start a second one; what a profile enables is then one set, whatever named the extension first.
 *
 * `specTag` is the name the GFM specification's own example fences carry (```` ```` example table ````), which is also
 * the name cmark-gfm registers the extension under. Vendored fixtures record it per example, so a conformance harness
 * can report an extension's score without knowing anything about how the extension is implemented.
 *
 * [[TaskListItems]] is the one whose examples carry no tag — the specification marks both of them `disabled`, because
 * cmark-gfm's own rendering of the checkbox input differs from what the prose shows. The tag is still cmark-gfm's
 * registered name, so the pairing stays uniform.
 *
 * @see
 *   [[https://github.github.com/gfm/ GitHub Flavored Markdown Spec, version 0.29-gfm]]
 */
enum MdExtension(val specTag: String) derives CanEqual:
  /** Pipe tables: a delimiter row of dashes and colons under a header row, with per-column alignment. */
  case Tables extends MdExtension("table")

  /** `[ ]` and `[x]` at the start of a list item's first paragraph, rendered as a disabled checkbox. */
  case TaskListItems extends MdExtension("tasklist")

  /** `~~struck~~`, a delimiter run parsed like emphasis and rendered as `del`. */
  case Strikethrough extends MdExtension("strikethrough")

  /** Bare URLs, `www.` hostnames and email addresses linked without `<>` around them. */
  case Autolinks extends MdExtension("autolink")

  /** Escaping of a fixed list of raw HTML tags — `script`, `style`, `iframe` and kin — rather than passing them through. */
  case TagFilter extends MdExtension("tagfilter")

object MdExtension:
  /** The five the GitHub Flavored Markdown specification defines, which is what [[MdProfile.gfm]] enables. */
  val gfm: Set[MdExtension] = values.toSet
