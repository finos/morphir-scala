package morphir.langkit.markdown.internal

import kyo.*
import scala.annotation.tailrec
import morphir.langkit.core.Span
import morphir.langkit.markdown.*

/**
 * GFM extended autolinks: `www.` hosts, bare scheme URLs and bare email addresses, recognised in already-parsed prose
 * rather than at inline-parse time.
 *
 * Split out of [[InlineParser]], which still calls [[extendAutolinks]] as the post-pass over its own output and carries
 * the three helpers this needs that are not extended-autolink specific: `normalizeUri`, `spanOf` and
 * `isUnicodeWhitespace`.
 */
private[internal] object ExtendedAutolinks:
  import InlineParser.{isUnicodeWhitespace, normalizeUri, spanOf}

  /**
   * Which of the three shapes a matched run has.
   *
   * Carried on the match rather than re-read off the run, because the three are not told apart by their prefixes: see
   * [[extendedDestinationOf]].
   */
  private[internal] enum ExtendedForm derives CanEqual:
    /** A `www.` host, which points at itself behind `http://`. */
    case Www

    /** A destination that names its own scheme, which points at itself. */
    case Scheme

    /** An address, which points at itself behind `mailto:`. */
    case Email

  /**
   * Where an extended autolink ends in the value it was found in, which character makes it one — the one a writer
   * spells as a reference to break it — and which shape it has.
   */
  private[internal] type ExtendedMatch = (end: Int, anchor: Int, form: ExtendedForm)

  /**
   * The three schemes the extension links bare, longest first so `https://` is never read as `http` and a stray `s`.
   */
  private val ExtendedSchemes: Chunk[String] = Chunk("https://", "http://", "ftp://")

  /**
   * The most a local part may run to, which is RFC 5321's own limit and the same standard the bracketed autolink's
   * [[InlineParser.isDomainLabel]] takes its 63 from.
   *
   * It is also what keeps this scan linear. `_` is both a boundary character and a local-part one, so every index of a
   * long underscore run is a position an address may begin at; without a bound, each of them would scan the rest of the
   * run looking for an `@`, and `"_" * 50000 + "."` would take quadratic time. A local part longer than this is not an
   * address by the standard, and is left as prose here where cmark-gfm would link it.
   */
  private val ExtendedLocalLimit = 64

  /**
   * Where an extended autolink points, for lowering, which has the matched run and nothing else.
   *
   * The form is recovered by matching the run again rather than by testing its prefix, and that distinction is the
   * whole of this method. `www.a@b.c` is an address whose local part happens to begin `www.` — the host form rejects
   * it, because `a` is a single-label domain — so a prefix test would answer `http://` where GFM answers `mailto:`.
   * Deriving it from the same matcher that recognised the run in the first place makes that class of disagreement
   * unrepresentable. A run that is not an extended autolink at all — only a hand-built tree holds one — keeps its text
   * and gains no scheme.
   */
  private[internal] def extendedDestinationOf(text: String): String =
    extendedAutolinkAt(text, 0, nodeStart = true) match
      case Present(matched) if matched.end == text.length => destinationOf(matched.form, text)
      case _                                              => normalizeUri(text)

  /** Where a run of a known form points: itself, `http://` in front of a host, `mailto:` in front of an address. */
  private def destinationOf(form: ExtendedForm, text: String): String =
    normalizeUri(form match
      case ExtendedForm.Scheme => text
      case ExtendedForm.Www    => "http://" + text
      case ExtendedForm.Email  => "mailto:" + text)

  /**
   * Split every text node into the extended autolinks it holds and the text around them.
   *
   * Runs over finished phrasing content rather than over the source, which is what keeps it out of code spans, links
   * and raw HTML for free: those are already nodes by the time this sees them, and a node that is not [[MdNode.Text]]
   * is passed through untouched. Emphasis, strong emphasis and strikethrough are transparent — their content is prose
   * that happens to be wrapped — while a link is not, because a link may not contain one.
   *
   * The spans it produces are real source offsets, so the concrete syntax tree tiles a link it makes exactly as it
   * tiles one the author bracketed.
   */
  private[internal] def extendAutolinks(
      content: Chunk[MdNode.PhrasingContent],
      text: String,
      sourceOffsetAt: Int => Int
  )(using profile: MdProfile): Chunk[MdNode.PhrasingContent] =
    if !profile.supports(MdExtension.Autolinks) then content
    else
      val out = Chunk.newBuilder[MdNode.PhrasingContent]

      /**
       * A run of text nodes that are adjacent in the block text, scanned as the one string they were written as.
       *
       * Inline parsing splits prose wherever a delimiter run came to nothing, so `a.b-c_d@a.b` arrives as three nodes
       * around its `_`. Scanning them one at a time would find an address beginning after the underscore, which is
       * exactly the wrong answer; scanning the run gives the address the author wrote. A run that holds no link keeps
       * the nodes the parse made, so nothing is re-segmented for nothing.
       */
      def flush(from: Int, until: Int, nodes: Chunk[MdNode.Text]): Unit =
        if nodes.nonEmpty then
          splitAutolinks(text, from, until, sourceOffsetAt) match
            case Present(split) => out.addAll(split)
            case Absent         => nodes.foreach(out.addOne)

      def passed(node: MdNode.PhrasingContent): MdNode.PhrasingContent = node match
        // Emphasis of every kind is transparent: its content is prose that happens to be wrapped. A link is not, and
        // neither is an image, whose alt text is already a string.
        case MdNode.Emphasis(inner, meta) => MdNode.Emphasis(extendAutolinks(inner, text, sourceOffsetAt), meta)
        case MdNode.Strong(inner, meta)   => MdNode.Strong(extendAutolinks(inner, text, sourceOffsetAt), meta)
        case MdNode.Delete(inner, meta)   => MdNode.Delete(extendAutolinks(inner, text, sourceOffsetAt), meta)
        case other                        => other

      @tailrec def loop(index: Int, from: Int, until: Int, nodes: Chunk[MdNode.Text]): Unit =
        if index >= content.length then flush(from, until, nodes)
        else
          val piece = content(index) match
            case node: MdNode.Text => textIndexOf(node.value, node.meta, text, sourceOffsetAt).map(at => (node, at))
            case _                 => Absent
          piece match
            case Present((node, at)) if nodes.isEmpty => loop(index + 1, at, at + node.value.length, Chunk(node))
            case Present((node, at)) if at == until   => loop(index + 1, from, at + node.value.length, nodes :+ node)
            case Present((node, at))                  =>
              flush(from, until, nodes)
              loop(index + 1, at, at + node.value.length, Chunk(node))
            case Absent =>
              flush(from, until, nodes)
              out.addOne(passed(content(index)))
              loop(index + 1, 0, 0, Chunk.empty)

      loop(0, 0, 0, Chunk.empty)
      out.result()
  end extendAutolinks

  /**
   * The block text over `[from, until)` as the links it holds and the text between them, or [[kyo.Absent]] when it
   * holds none — which is the caller's cue to keep the nodes the parse already made.
   */
  private def splitAutolinks(
      text: String,
      from: Int,
      until: Int,
      sourceOffsetAt: Int => Int
  ): Maybe[Chunk[MdNode.PhrasingContent]] =
    val value = text.substring(from, until)
    // Every form the extension recognises has a period in it -- a `www.` host, a scheme's domain and an address's
    // domain each need one -- so a value without one cannot hold a match and is not worth scanning. Most runs take
    // this exit.
    if value.indexOf('.') < 0 then Absent
    else
      val out                                     = Chunk.newBuilder[MdNode.PhrasingContent]
      def spanOfValue(start: Int, end: Int): Span = spanOf(from + start, from + end, sourceOffsetAt)
      def emitText(start: Int, end: Int): Unit    =
        if end > start then out.addOne(MdNode.Text(value.substring(start, end), MdMeta.at(spanOfValue(start, end))))
      def emitLink(start: Int, end: Int, form: ExtendedForm): Unit =
        val span = spanOfValue(start, end)
        val run  = value.substring(start, end)
        out.addOne(MdNode.Link(
          destinationOf(form, run),
          Absent,
          Chunk(MdNode.Text(run, MdMeta.at(span))),
          MdMeta.at(span)
        ))
      @tailrec def loop(index: Int, pending: Int, found: Boolean): Boolean =
        if index >= value.length then
          emitText(pending, value.length)
          found
        else
          extendedAutolinkAt(value, index, nodeStart = false) match
            case Present(matched) =>
              emitText(pending, index)
              emitLink(index, matched.end, matched.form)
              loop(matched.end, matched.end, true)
            case Absent => loop(index + 1, pending, found)
      if loop(0, 0, false) then Present(out.result()) else Absent

  /**
   * The value's start offset in the block text, for a node that is a verbatim slice of it.
   *
   * The node's span is a *source* offset while the scan works in value offsets, and the two are not one subtraction
   * apart: a block whose text was joined from several lines dropped each continuation's container marker, so the source
   * it covers is longer than the text it became. Finding the text index and mapping every piece back through the same
   * `sourceOffsetAt` the rest of inline parsing uses is what keeps a link inside a quote pointing at the bytes the
   * author wrote. The search is a bisection because `sourceOffsetAt` is strictly increasing.
   *
   * [[kyo.Absent]] when the node is not a verbatim slice of the text. A value holding a resolved backslash escape is
   * shorter than the source it stands for, so no offset within it can be trusted and the node keeps its text — which is
   * also what lets an escape spell a bare URL that stays prose.
   */
  private def textIndexOf(value: String, meta: MdMeta, text: String, sourceOffsetAt: Int => Int): Maybe[Int] =
    meta.span.flatMap { span =>
      @tailrec def loop(low: Int, high: Int): Maybe[Int] =
        if low > high then Absent
        else
          val middle = (low + high) / 2
          val offset = sourceOffsetAt(middle)
          if offset == span.offset then Present(middle)
          else if offset < span.offset then loop(middle + 1, high)
          else loop(low, middle - 1)
      loop(0, text.length).flatMap { from =>
        if from + value.length <= text.length && text.startsWith(value, from) &&
          sourceOffsetAt(from + value.length) == span.end
        then Present(from)
        else Absent
      }
    }

  /**
   * The extended autolink beginning at `at`, if one does.
   *
   * The three forms are tried in the order the specification lists them. All of them are recognised only at the start
   * of the text, after whitespace, or after one of `*`, `_`, `~` and `(` — the specification's own list, which is what
   * keeps the `www.` in `a-www.example.com` from being a link and lets the one in `(www.example.com)` be one.
   *
   * `nodeStart` says the character at `at` opens a text node of its own, which is a boundary whatever precedes it in
   * this string. The scan needs it only at `at == 0`, where the test is free; [[MdWriter]] passes it after writing a
   * character reference, because a reference is its own node in a parse and so opens one.
   *
   * `end` bounds how far a match may reach; `-1` (the default) means the whole string. [[MdWriter]]'s escaper passes a
   * narrower one: a segment it is about to close with an escape of its own — for a reason that has nothing to do with
   * autolinks — becomes its own node in a reparse, so a match has to be complete *within* that segment to be one a
   * reparse will actually see. Checking the unbounded string would find matches a reparse never will, and miss the
   * mirror case: a prefix invalid over the whole string only because of what comes after the cut.
   *
   * `end` is by-name because finding it can itself be a scan of the string, as it is for [[MdWriter]]'s caller. The
   * guard above rejects most positions in a long run without ever reading past `at - 1`, and evaluating `end` strictly
   * would force that scan at every rejected position too, turning a linear pass over the text quadratic.
   */
  private[internal] def extendedAutolinkAt(
      value: String,
      at: Int,
      nodeStart: Boolean,
      end: => Int = -1
  ): Maybe[ExtendedMatch] =
    if at > 0 && !nodeStart && !isExtendedBoundary(value.charAt(at - 1)) then Absent
    else
      val endValue = end
      val bounded  = if endValue < 0 || endValue == value.length then value else value.substring(0, endValue)
      wwwAutolinkAt(bounded, at) match
        case found @ Present(_) => found
        case Absent             =>
          urlAutolinkAt(bounded, at) match
            case found @ Present(_) => found
            case Absent             => emailAutolinkAt(bounded, at)

  private def isExtendedBoundary(char: Char): Boolean =
    isUnicodeWhitespace(char) || char == '*' || char == '_' || char == '~' || char == '('

  /** `www.` and a valid domain, then anything that is not whitespace or `<`; the anchor is the `.` of `www.`. */
  private def wwwAutolinkAt(value: String, at: Int): Maybe[ExtendedMatch] =
    if !value.regionMatches(true, at, "www.", 0, 4) then Absent
    else
      domainEnd(value, at + 4).flatMap { _ =>
        val end = trimmedEnd(value, at, extendedRunEnd(value, at))
        if end > at + 4 then Present((end = end, anchor = at + 3, form = ExtendedForm.Www)) else Absent
      }

  /**
   * A scheme, a valid domain, then anything that is not whitespace or `<`; the anchor is the scheme's `:`.
   *
   * The domain must be valid here too, which the specification states and which makes `http://localhost/x` prose —
   * cmark-gfm is laxer about that one, and the published examples do not decide between them, so the specification's
   * own words win.
   */
  private def urlAutolinkAt(value: String, at: Int): Maybe[ExtendedMatch] =
    Maybe.fromOption(ExtendedSchemes.find(scheme => value.regionMatches(true, at, scheme, 0, scheme.length)))
      .flatMap { scheme =>
        domainEnd(value, at + scheme.length).flatMap { _ =>
          val end = trimmedEnd(value, at, extendedRunEnd(value, at))
          if end > at + scheme.length then
            Present((end = end, anchor = at + scheme.indexOf(':'), form = ExtendedForm.Scheme))
          else Absent
        }
      }

  /**
   * An address: a local part, an `@`, and a domain that ends the match. The anchor is the `@`.
   *
   * No trailing trim runs over this form, because its own domain rule already says where it ends: a period counts only
   * when something alphanumeric follows it, which leaves the full stop of `a.b-c_d@a.b.` outside the address without
   * any trimming at all.
   *
   * The local part is read forwards from a boundary, where cmark-gfm rewinds from the `@` over local characters and
   * asks nothing of what precedes them. The difference shows in `path/foo@bar.baz`, prose here and a link there: a `/`
   * is not one of the four characters the specification names, and its blanket sentence — every extended autolink "can
   * only come at the beginning of a line, after whitespace, or any of the delimiting characters `*`, `_`, `~` and `(`"
   * — is written for all three forms. The published examples do not decide between the two readings.
   */
  private def emailAutolinkAt(value: String, at: Int): Maybe[ExtendedMatch] =
    @tailrec def loop(index: Int): Int =
      if index < value.length && index - at < ExtendedLocalLimit && isExtendedEmailLocal(value.charAt(index)) then
        loop(index + 1)
      else index
    val sign = loop(at)
    if sign == at || sign >= value.length || value.charAt(sign) != '@' then Absent
    else emailDomainEnd(value, sign + 1).map(end => (end = end, anchor = sign, form = ExtendedForm.Email))

  private def isExtendedEmailLocal(char: Char): Boolean =
    char.isLetterOrDigit || char == '.' || char == '-' || char == '_' || char == '+'

  /**
   * Where a `www.` or scheme host's domain ends, or [[kyo.Absent]] when what is there is not a domain.
   *
   * Segments of alphanumerics, hyphens and underscores separated by periods. There must be at least one period, so
   * `www.foo` is prose, and no underscore may appear in either of the last two segments.
   */
  private def domainEnd(value: String, from: Int): Maybe[Int] =
    @tailrec def loop(index: Int): Int =
      if index < value.length && isDomainCharacter(value.charAt(index)) then loop(index + 1) else index
    val end    = loop(from)
    val labels = value.substring(from, end).split("\\.", -1)
    if end == from || labels.length < 2 || labels.takeRight(2).exists(_.indexOf('_') >= 0) then Absent
    else Present(end)

  private def isDomainCharacter(char: Char): Boolean =
    char.isLetterOrDigit || char == '-' || char == '_' || char == '.'

  /**
   * Where an address's domain ends, or [[kyo.Absent]] when it is not one.
   *
   * Labels of alphanumerics, hyphens and underscores separated by periods, with at least one period, and a period only
   * counts when an alphanumeric follows it. The last character may be neither `-` nor `_`, and that rule disqualifies
   * the whole match rather than trimming: `a.b-c_d@a.b-` holds no link at all, where `a.b-c_d@a.b.` holds one that
   * stops before the full stop.
   */
  private def emailDomainEnd(value: String, from: Int): Maybe[Int] =
    @tailrec def loop(index: Int, periods: Int): (end: Int, periods: Int) =
      if index >= value.length then (end = index, periods = periods)
      else
        val char = value.charAt(index)
        if char.isLetterOrDigit || char == '-' || char == '_' then loop(index + 1, periods)
        else if char == '.' && index + 1 < value.length && value.charAt(index + 1).isLetterOrDigit then
          loop(index + 1, periods + 1)
        else (end = index, periods = periods)
    val scanned = loop(from, 0)
    if scanned.end == from || scanned.periods == 0 then Absent
    else
      val last = value.charAt(scanned.end - 1)
      if last == '-' || last == '_' then Absent else Present(scanned.end)

  /** Past everything a destination may hold once its domain is behind it: anything but whitespace and `<`. */
  @tailrec private def extendedRunEnd(value: String, index: Int): Int =
    if index < value.length && !isUnicodeWhitespace(value.charAt(index)) && value.charAt(index) != '<' then
      extendedRunEnd(value, index + 1)
    else index

  /**
   * Where the run really ends once its tail is given back, which is where nearly all of the extension's difficulty
   * lives. Three rules, applied repeatedly until none of them fires:
   *
   *   - trailing `?`, `!`, `.`, `,`, `:`, `*`, `_` and `~` are not part of the link, so a link at the end of a sentence
   *     does not swallow the full stop;
   *   - a trailing `)` is given back only while the run holds more of them than it holds `(`, which is what lets
   *     `www.google.com/search?q=Markup+(business)` keep its pair inside a sentence that parenthesises it;
   *   - a trailing `;` preceded by `&` and one or more alphanumerics is an entity reference and comes off whole.
   *
   * The order is what settles the interactions. A run ending `);` stops at the semicolon rule — a `)` is not part of an
   * entity name — and keeps both characters, and a run ending `.)` gives the parenthesis back first and then looks at
   * the full stop the parenthesis was hiding.
   *
   * The parentheses are counted once, here, and the count is carried through the loop rather than retaken per step.
   * Retaking it made a run of trailing `)` quadratic, and the count stays true for free: neither of the other two rules
   * can remove a parenthesis, since none of the eight punctuation characters is one and an entity reference is an `&`,
   * alphanumerics and a `;`.
   */
  private def trimmedEnd(value: String, start: Int, end: Int): Int =
    @tailrec def counted(index: Int, opening: Int, closing: Int): (opening: Int, closing: Int) =
      if index >= end then (opening = opening, closing = closing)
      else
        value.charAt(index) match
          case '(' => counted(index + 1, opening + 1, closing)
          case ')' => counted(index + 1, opening, closing + 1)
          case _   => counted(index + 1, opening, closing)
    val parentheses = counted(start, 0, 0)
    trimmedTail(value, start, end, parentheses.opening, parentheses.closing)

  @tailrec private def trimmedTail(value: String, start: Int, end: Int, opening: Int, closing: Int): Int =
    if end <= start then end
    else
      value.charAt(end - 1) match
        case '?' | '!' | '.' | ',' | ':' | '*' | '_' | '~' => trimmedTail(value, start, end - 1, opening, closing)
        case ')' if closing > opening                      => trimmedTail(value, start, end - 1, opening, closing - 1)
        case ';'                                           =>
          entityReferenceStart(value, start, end) match
            case Present(ampersand) => trimmedTail(value, start, ampersand, opening, closing)
            case Absent             => end
        case _ => end

  /**
   * Where the entity reference ending at `end` begins, when the `;` there closes one.
   *
   * "Resembles an entity reference" is all the specification asks for: an `&`, one or more alphanumerics, and the
   * semicolon. Whether the name is one HTML knows is not checked, and example 626 is why — `&hl;` names nothing and is
   * excluded all the same.
   */
  private def entityReferenceStart(value: String, start: Int, end: Int): Maybe[Int] =
    @tailrec def loop(index: Int): Int =
      if index > start && value.charAt(index - 1).isLetterOrDigit then loop(index - 1) else index
    val nameStart = loop(end - 1)
    if nameStart < end - 1 && nameStart > start && value.charAt(nameStart - 1) == '&' then Present(nameStart - 1)
    else Absent
