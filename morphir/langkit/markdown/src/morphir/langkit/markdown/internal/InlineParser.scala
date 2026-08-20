package morphir.langkit.markdown.internal

import kyo.*
import scala.annotation.tailrec
import morphir.langkit.core.Span
import morphir.langkit.markdown.*

/**
 * Splits a block's raw prose into [[Inline]] nodes.
 *
 * Internal on purpose: blocks reach callers already carrying inline content, and no caller runs this itself. Each
 * construct this learns to recognise turns text that used to be literal into a typed node, so the set of cases grows
 * while the entry point does not.
 */
/** A link reference definition: what a `[label]: destination "title"` line contributes. */
private[markdown] final case class LinkDefinition(destination: String, title: Maybe[String])

private[markdown] object InlineParser:

  /**
   * Where a link points, and where the construct that named it ends.
   *
   * Named because three of these in a row said nothing about which was which: `(Int, String, Maybe[String])` reads the
   * same whichever way round the caller has it.
   */
  private[markdown] type Target = (end: Int, destination: String, title: Maybe[String])

  /** An autolink: where it ends, what it says, and where it points -- which differ for the email form. */
  private type Autolink = (end: Int, text: String, destination: String)

  /** Where a label opened at `open` closes; used by block parsing to find a definition's label. */
  def labelEndOf(text: String, open: Int): Maybe[Int] = labelEnd(text, open)

  /**
   * A definition's `destination "title"` tail, which must be followed by nothing but whitespace.
   *
   * Stricter than an inline link's target: example 209 shows `[foo]: /url "title" ok` is not a definition at all,
   * because trailing content disqualifies it.
   */
  def definitionTarget(text: String, from: Int): Maybe[Target] =
    definitionTargetImpl(text, from)

  /**
   * Normalise a link label the way the spec matches them: trimmed, whitespace collapsed, case folded.
   *
   * Case *folding*, which is not lowercasing. `ẞ` lowercases to `ß` and would never meet `SS`, but both fold to `ss`,
   * so `[ẞ]` finds a definition written `[SS]`. Going down, up, then down again is how that is reached with what the
   * standard library offers on all three platforms.
   */
  def normalizeLabel(label: String): String =
    label.trim.replaceAll("\\s+", " ").toLowerCase.toUpperCase.toLowerCase

  /** Where a link label opening at `open` closes, for a caller outside this object. */
  def referenceLabelEndOf(text: String, open: Int): Maybe[Int] = referenceLabelEnd(text, open)

  /** Whether a bracketed run may serve as a link label at all. */
  def isLabelOf(text: String): Boolean = isLabel(text)

  /** Resolve backslash escapes and character references, for a caller outside this object. */
  def resolveEscapes(value: String): String = unescape(value)

  /**
   * Parse `text` into inline nodes.
   *
   * @param text
   *   the block's prose, with its marker and surrounding whitespace already removed
   * @param sourceOffsetAt
   *   maps an index in `text` to its offset in the original source. A block whose text was joined from several lines
   *   supplies a mapping that accounts for the line endings it dropped, so spans stay true even when the joined text is
   *   shorter than the source it came from.
   */
  def parse(
      text: String,
      sourceOffsetAt: Int => Int,
      definitions: Map[String, LinkDefinition] = Map.empty
  ): Chunk[Inline] =
    val items = scanItems(text, sourceOffsetAt, definitions)
    processEmphasis(items, sourceOffsetAt)
    Chunk.from(items.filterNot(_.dropped).map(item => item.inline.getOrElse(literal(item, sourceOffsetAt))))

  /**
   * One position in the inline stream: either a finished node, or a delimiter run still deciding what it is.
   *
   * Mutable because the emphasis algorithm consumes delimiters a pair at a time and may leave part of a run behind:
   * `*foo**` matches one `*` and leaves the other as text.
   */
  private final class Item(
      val inline: Maybe[Inline],
      val delimiter: Char,
      var count: Int,
      val originalCount: Int,
      val canOpen: Boolean,
      val canClose: Boolean,
      val start: Int,
      val end: Int,
      var dropped: Boolean = false
  ):
    def isDelimiter: Boolean = inline.isEmpty

  /** What an unmatched delimiter run turns into: the characters it was made of. */
  private def literal(item: Item, sourceOffsetAt: Int => Int): Inline =
    // A run consumed a pair at a time leaves its tail behind, so the span narrows to what is left.
    val start = item.end - item.count
    Inline.Text(item.delimiter.toString * item.count, spanOf(start, item.end, sourceOffsetAt))

  /** Pass one: constructs become nodes, `*`/`_` runs become delimiters, everything else accumulates as text. */
  private def scanItems(
      text: String,
      sourceOffsetAt: Int => Int,
      definitions: Map[String, LinkDefinition]
  ): scala.collection.mutable.ArrayBuffer[Item] =
    val items        = scala.collection.mutable.ArrayBuffer.empty[Item]
    val pending      = StringBuilder()
    var pendingStart = 0
    var index        = 0

    def node(inline: Inline): Item = Item(Present(inline), ' ', 0, 0, false, false, 0, 0)

    def flushPending(): Unit =
      if pending.nonEmpty then
        items += node(Inline.Text(pending.toString, spanOf(pendingStart, index, sourceOffsetAt)))
        pending.clear()

    while index < text.length do
      val char = text.charAt(index)
      if char == '\\' && index + 1 < text.length && isAsciiPunctuation(text.charAt(index + 1)) then
        // A backslash escape makes the next character literal, so it can never open or close anything.
        if pending.isEmpty then pendingStart = index
        pending.append(text.charAt(index + 1))
        index += 2
      else
        constructAt(text, index, sourceOffsetAt, definitions) match
          case Present((constructEnd, inline)) =>
            flushPending()
            items += node(inline)
            index = constructEnd
            pendingStart = index
          case Absent =>
            if char == '*' || char == '_' then
              val run = delimiterRun(text, index, char)
              flushPending()
              items += run
              index = run.end
              pendingStart = index
            else
              val run = if char == '`' then backtickRun(text, index) else 1
              if pending.isEmpty then pendingStart = index
              pending.append(text.substring(index, index + run))
              index += run
    end while
    flushPending()
    items
  end scanItems

  /**
   * A run of `*` or `_`, classified by the spec's flanking rules.
   *
   * Left-flanking means the run is not followed by whitespace, and either is not followed by punctuation or is preceded
   * by whitespace or punctuation; right-flanking is the mirror. `_` is stricter than `*` so that intraword underscores
   * stay literal, which is why `foo_bar_` is not emphasis.
   */
  private def delimiterRun(text: String, start: Int, char: Char): Item =
    var end = start
    while end < text.length && text.charAt(end) == char do end += 1
    val length = end - start

    val before = if start == 0 then ' ' else text.charAt(start - 1)
    val after  = if end >= text.length then ' ' else text.charAt(end)

    val beforeWhitespace = isUnicodeWhitespace(before)
    val afterWhitespace  = isUnicodeWhitespace(after)
    val beforePunct      = isUnicodePunctuation(before)
    val afterPunct       = isUnicodePunctuation(after)

    val leftFlanking  = !afterWhitespace && (!afterPunct || beforeWhitespace || beforePunct)
    val rightFlanking = !beforeWhitespace && (!beforePunct || afterWhitespace || afterPunct)

    val canOpen  = if char == '*' then leftFlanking else leftFlanking && (!rightFlanking || beforePunct)
    val canClose = if char == '*' then rightFlanking else rightFlanking && (!leftFlanking || afterPunct)

    Item(Absent, char, length, length, canOpen, canClose, start, end)
  end delimiterRun

  /**
   * Pass two: the spec's process-emphasis procedure.
   *
   * Walks forward to each potential closer, looks back for the nearest matching opener, and wraps everything between
   * them. Two or more delimiters on both sides make strong emphasis and consume two; otherwise one each. A run may be
   * consumed a pair at a time, which is what lets `*foo**bar***` nest.
   */
  private def processEmphasis(
      items: scala.collection.mutable.ArrayBuffer[Item],
      sourceOffsetAt: Int => Int
  ): Unit =
    // The spec's "openers bottom": below this index, a closer of this shape has already failed to find an opener.
    val openersBottom = scala.collection.mutable.Map.empty[(Char, Int, Boolean), Int]
    var closerIndex   = 0

    while closerIndex < items.length do
      val closer = items(closerIndex)
      if !closer.dropped && closer.isDelimiter && closer.canClose then
        val key    = (closer.delimiter, closer.originalCount % 3, closer.canOpen)
        val bottom = openersBottom.getOrElse(key, -1)

        var openerIndex = closerIndex - 1
        var found       = -1
        while found < 0 && openerIndex > bottom do
          val candidate = items(openerIndex)
          if !candidate.dropped && candidate.isDelimiter && candidate.canOpen &&
            candidate.delimiter == closer.delimiter && ruleOfThree(candidate, closer)
          then found = openerIndex
          openerIndex -= 1

        if found >= 0 then
          val opener = items(found)
          val strong = opener.count >= 2 && closer.count >= 2
          val used   = if strong then 2 else 1

          val inner = Chunk.from(
            items.slice(found + 1, closerIndex).filterNot(_.dropped).map(item =>
              item.inline.getOrElse(literal(item, sourceOffsetAt))
            )
          )
          items.slice(found + 1, closerIndex).foreach(_.dropped = true)

          val span = spanOf(opener.start, closer.end, sourceOffsetAt)
          val node = if strong then Inline.StrongEmphasis(inner, span) else Inline.Emphasis(inner, span)
          items.insert(closerIndex, Item(Present(node), ' ', 0, 0, false, false, 0, 0))
          closerIndex += 1

          opener.count -= used
          closer.count -= used
          if opener.count == 0 then opener.dropped = true
          if closer.count == 0 then
            closer.dropped = true
            closerIndex += 1
        else
          // No opener for this closer: remember how far back is pointless to search next time.
          openersBottom(key) = closerIndex - 1
          // The spec removes it from the delimiter stack, not from the text. Turning it into a node does both: it
          // stops being a closer, and its characters still render.
          if !closer.canOpen then
            items(closerIndex) = Item(Present(literal(closer, sourceOffsetAt)), ' ', 0, 0, false, false, 0, 0)
          closerIndex += 1
      else closerIndex += 1
    end while
  end processEmphasis

  /**
   * The spec's rule of three.
   *
   * When a delimiter can both open and close, the two run lengths may not sum to a multiple of three unless both are
   * themselves multiples of three. It is what stops `*foo**bar*` from pairing the wrong delimiters.
   */
  private def ruleOfThree(opener: Item, closer: Item): Boolean =
    val ambiguous = opener.canClose || closer.canOpen
    if !ambiguous then true
    else if (opener.originalCount + closer.originalCount) % 3 != 0 then true
    else opener.originalCount                             % 3 == 0 && closer.originalCount % 3 == 0

  /** The inline construct beginning at `index`, if any, and the index just past it. */
  private def constructAt(
      text: String,
      index: Int,
      sourceOffsetAt: Int => Int,
      definitions: Map[String, LinkDefinition]
  ): Maybe[(Int, Inline)] =
    val char = text.charAt(index)
    if char == ' ' || char == '\\' then lineEndingAt(text, index, sourceOffsetAt)
    else if char == '`' then
      val run = backtickRun(text, index)
      closingRun(text, index + run, run).map { closeStart =>
        val end = closeStart + run
        (end, Inline.CodeSpan(normalize(text.substring(index + run, closeStart)), spanOf(index, end, sourceOffsetAt)))
      }
    else if char == '&' then entityAt(text, index, sourceOffsetAt)
    else if char == '<' then
      autolink(text, index) match
        case Present(link) =>
          val span = spanOf(index, link.end, sourceOffsetAt)
          Present((
            link.end,
            Inline.Link(normalizeUri(link.destination), Absent, Chunk(Inline.Text(link.text, span)), span)
          ))
        case Absent =>
          // Raw HTML is taken whole, which is what gives it precedence over everything else: no `*`, `_`, `&` or `\`
          // inside it is ever looked at again, so `<a href="&ouml;">` keeps its entity and `**<a href="**">` has no
          // emphasis in it.
          HtmlTag.endOf(text, index).map { end =>
            (end, Inline.RawHtml(text.substring(index, end), spanOf(index, end, sourceOffsetAt)))
          }
    else if isLinkStart(text, index) then
      val image = char == '!'
      val open  = if image then index + 2 else index + 1
      linkAt(text, index, open, image, sourceOffsetAt, definitions)
    else Absent

  /**
   * A line ending, and what the author asked of it.
   *
   * Two or more spaces before it, or a backslash before it, is a hard break and becomes a node. One space is not: it is
   * dropped, because trailing whitespace on a line is not content. Either way the whitespace is consumed here rather
   * than stripped beforehand -- stripping first would throw away the very thing that decides which of the two this is.
   *
   * A run of spaces that does not reach a line ending is ordinary text, and a backslash before anything but a line
   * ending is left for the escape rules.
   */
  private def lineEndingAt(text: String, start: Int, sourceOffsetAt: Int => Int): Maybe[(Int, Inline)] =
    if text.charAt(start) == '\\' then
      if start + 1 < text.length && text.charAt(start + 1) == '\n' then
        Present((start + 2, Inline.LineBreak(spanOf(start, start + 2, sourceOffsetAt))))
      else Absent
    else
      @tailrec def spacesEnd(index: Int): Int =
        if index < text.length && text.charAt(index) == ' ' then spacesEnd(index + 1) else index
      val end = spacesEnd(start)
      if end >= text.length || text.charAt(end) != '\n' then Absent
      else if end - start >= 2 then Present((end + 1, Inline.LineBreak(spanOf(start, end + 1, sourceOffsetAt))))
      // One space before a line ending is neither a break nor content: the line ending stands on its own.
      else Present((end + 1, Inline.Text("\n", spanOf(start, end + 1, sourceOffsetAt))))

  /**
   * A character reference: `&name;`, `&#dddd;` or `&#xhhhh;`.
   *
   * Decoded here rather than passed through, so the writer escapes the decoded character on its own terms: `&amp;`
   * becomes a text node holding `&`, which ScalaTags then writes back as `&amp;`. Anything that does not parse cleanly
   * stays literal text, which is what makes `&MadeUpEntity;` and a bare `&copy` render as themselves.
   */
  private def entityAt(text: String, start: Int, sourceOffsetAt: Int => Int): Maybe[(Int, Inline)] =
    entityValue(text, start).map { case (end, value) =>
      (end, Inline.Text(value, spanOf(start, end, sourceOffsetAt)))
    }

  /** The character a reference beginning at `start` stands for, and where it ends. */
  private def entityValue(text: String, start: Int): Maybe[(Int, String)] =
    val semicolon = text.indexOf(';', start + 1)
    if semicolon < 0 || semicolon == start + 1 then Absent
    else
      val body    = text.substring(start + 1, semicolon)
      val decoded =
        if body.startsWith("#") then numericEntity(body.drop(1))
        else NamedEntities.get(body)
      decoded match
        case Some(value) => Present((semicolon + 1, value))
        case None        => Absent

  /**
   * A numeric reference, decimal or hexadecimal.
   *
   * U+0000 is not representable and the spec replaces it with U+FFFD, as it does anything out of range. Digits are
   * bounded so a long run cannot be used to force a large allocation.
   */
  private def numericEntity(digits: String): Option[String] =
    val (body, radix, limit) =
      if digits.startsWith("x") || digits.startsWith("X") then (digits.drop(1), 16, 6) else (digits, 10, 7)
    // Seven digits decimal and six hexadecimal, which is the spec's bound and not merely a guard against a long run.
    // A longer one is not an out-of-range reference to be replaced with U+FFFD; it is not a reference at all, and
    // `&#87654321;` stays the text the author wrote.
    val valid =
      body.nonEmpty && body.length <= limit &&
        body.forall(char => if radix == 16 then isHexDigit(char) else char.isDigit)
    if !valid then None
    else
      val code = java.lang.Long.parseLong(body, radix)
      if code == 0L || code > Character.MAX_CODE_POINT.toLong || Character.isSurrogate(code.toChar) then
        Some("\uFFFD")
      else Some(new String(Character.toChars(code.toInt)))

  private def isHexDigit(char: Char): Boolean =
    char.isDigit || (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F')

  /**
   * The named references this parser knows.
   *
   * HTML5 defines around 2,231 of them and CommonMark expects all of them. This is the working subset; the rest is a
   * data-import job rather than a parsing one, and the examples that need `&Dcaron;` or `&ClockwiseContourIntegral;`
   * stay failing until that lands. An unknown name is left as literal text, so being incomplete is safe rather than
   * wrong.
   */
  private val NamedEntities: Map[String, String] = Map(
    "amp"    -> "&",
    "lt"     -> "<",
    "gt"     -> ">",
    "quot"   -> "\"",
    "apos"   -> "'",
    "nbsp"   -> "\u00a0",
    "copy"   -> "\u00a9",
    "reg"    -> "\u00ae",
    "trade"  -> "\u2122",
    "hellip" -> "\u2026",
    "mdash"  -> "\u2014",
    "ndash"  -> "\u2013",
    "lsquo"  -> "\u2018",
    "rsquo"  -> "\u2019",
    "ldquo"  -> "\u201c",
    "rdquo"  -> "\u201d",
    "laquo"  -> "\u00ab",
    "raquo"  -> "\u00bb",
    "deg"    -> "\u00b0",
    "plusmn" -> "\u00b1",
    "times"  -> "\u00d7",
    "divide" -> "\u00f7",
    "frac12" -> "\u00bd",
    "frac14" -> "\u00bc",
    "frac34" -> "\u00be",
    "sup2"   -> "\u00b2",
    "sup3"   -> "\u00b3",
    "micro"  -> "\u00b5",
    "para"   -> "\u00b6",
    "sect"   -> "\u00a7",
    "dagger" -> "\u2020",
    "bull"   -> "\u2022",
    "euro"   -> "\u20ac",
    "pound"  -> "\u00a3",
    "yen"    -> "\u00a5",
    "cent"   -> "\u00a2",
    "AElig"  -> "\u00c6",
    "aelig"  -> "\u00e6",
    "auml"   -> "\u00e4",
    "ouml"   -> "\u00f6",
    "uuml"   -> "\u00fc",
    "szlig"  -> "\u00df",
    "eacute" -> "\u00e9",
    "egrave" -> "\u00e8",
    "larr"   -> "\u2190",
    "rarr"   -> "\u2192",
    "harr"   -> "\u2194",
    "hArr"   -> "\u21d4",
    "le"     -> "\u2264",
    "ge"     -> "\u2265",
    "ne"     -> "\u2260",
    "asymp"  -> "\u2248",
    "infin"  -> "\u221e"
  )

  private def isLinkStart(text: String, index: Int): Boolean =
    text.charAt(index) == '[' ||
      (text.charAt(index) == '!' && index + 1 < text.length && text.charAt(index + 1) == '[')

  /**
   * Parse a link or image beginning at `start`, whose label opens at `open`.
   *
   * Returns the index just past the construct and the node, or [[kyo.Absent]] when the text is not a link after all —
   * in which case the bracket is ordinary text.
   */
  private def linkAt(
      text: String,
      start: Int,
      open: Int,
      image: Boolean,
      sourceOffsetAt: Int => Int,
      definitions: Map[String, LinkDefinition]
  ): Maybe[(Int, Inline)] =
    labelEnd(text, open) match
      case Absent         => Absent
      case Present(close) =>
        val label = text.substring(open, close)

        def build(end: Int, destination: String, title: Maybe[String], normalize: Boolean): Maybe[(Int, Inline)] =
          val span = spanOf(start, end, sourceOffsetAt)
          val uri  = if normalize then normalizeUri(destination) else destination
          if image then Present((end, Inline.Image(uri, title, plainText(label, definitions), span)))
          else
            val content = parse(label, index => sourceOffsetAt(open + index), definitions)
            // Links may not contain links. The bracket that would have opened this one is ordinary text instead, and
            // the link inside it stands: `[foo [bar](/uri)](/uri)` is the word `[foo `, a link, then `](/uri)`.
            // Images are not bound by this -- their content becomes `alt` text, where a nested link flattens to what
            // it says.
            if content.exists(holdsLink) then Absent
            else Present((end, Inline.Link(uri, title, content, span)))

        val inlineForm =
          if close + 1 < text.length && text.charAt(close + 1) == '(' then
            inlineTarget(text, close + 2) match
              case Present(target) => build(target.end, target.destination, target.title, true)
              case Absent          => Absent
          else Absent

        if inlineForm.isDefined then inlineForm
        else referenceForm(text, open, close, label, sourceOffsetAt, definitions, build)

  /**
   * The three reference forms: `[text][label]`, the collapsed `[text][]`, and the shortcut `[text]`.
   *
   * All three resolve against definitions the document declares anywhere, including after the use, which is why block
   * parsing collects every definition before any inline content is parsed.
   */
  private def referenceForm(
      text: String,
      open: Int,
      close: Int,
      label: String,
      sourceOffsetAt: Int => Int,
      definitions: Map[String, LinkDefinition],
      build: (Int, String, Maybe[String], Boolean) => Maybe[(Int, Inline)]
  ): Maybe[(Int, Inline)] =
    val explicit =
      if close + 1 < text.length && text.charAt(close + 1) == '[' then referenceLabelEnd(text, close + 2)
      else Absent

    val (referenceLabel, end) = explicit match
      case Present(secondClose) =>
        val named = text.substring(close + 2, secondClose)
        // `[text][]` is the collapsed form: an empty second label means the first one is doing the naming.
        ((if named.isBlank then label else named), secondClose + 1)
      case Absent => (label, close + 1)

    // The shortcut and collapsed forms take the link text as their label, and the text may hold what a label may not.
    if !isLabel(referenceLabel) then Absent
    else
      definitions.get(normalizeLabel(referenceLabel)) match
        // Normalised here rather than when the definition was recorded, so a definition keeps the destination the author
        // wrote and every use of it is encoded the same way an inline destination would be: `/φου` reaches the writer as
        // `/%CF%86%CE%BF%CF%85`.
        case Some(definition) => build(end, definition.destination, definition.title, true)
        case None             => Absent

  /**
   * Where a link *label* closes: at the first `]` that no backslash escapes.
   *
   * Deliberately not [[labelEnd]], which is where link *text* closes. Text may hold balanced brackets, so
   * `[link [foo [bar]]](/uri)` is one link; a label may hold none, so `[foo][ref[bar]]` is not a link and
   * `[ref[bar]]: /uri` is not a definition. [[kyo.Absent]] when a bracket turns up that disqualifies it.
   */
  private def referenceLabelEnd(text: String, open: Int): Maybe[Int] =
    @tailrec def scan(index: Int): Maybe[Int] =
      if index >= text.length then Absent
      else
        text.charAt(index) match
          case '\\' => scan(index + 2)
          case ']'  => Present(index)
          case '['  => Absent
          case _    => scan(index + 1)
    scan(open)

  /**
   * Whether a run of text may serve as a link label.
   *
   * It needs something in it that is not whitespace -- `[]` and a label of nothing but a line break are not labels --
   * and no bracket that a backslash does not escape. The bracket rule matters for the shortcut form, whose label is the
   * link text and so may arrive holding brackets that a label may not: `[[[foo]]]` is text, not a link.
   */
  private def isLabel(text: String): Boolean =
    @tailrec def scan(index: Int, sawContent: Boolean): Boolean =
      if index >= text.length then sawContent
      else
        text.charAt(index) match
          case '\\'                => scan(index + 2, true)
          case '[' | ']'           => false
          case c if c.isWhitespace => scan(index + 1, sawContent)
          case _                   => scan(index + 1, true)
    scan(0, false)

  /**
   * Where the label that opened at `open` closes.
   *
   * Brackets nest, so `[link [foo [bar]]]` closes at the last one. Code spans, autolinks and raw HTML all bind tighter
   * than a link, so a bracket inside one is skipped rather than counted — which is why `[not a `link](/foo`)` is a code
   * span, and why `[foo <bar attr="](baz)">` is not a link at all: the `]` belongs to the attribute.
   */
  private def labelEnd(text: String, open: Int): Maybe[Int] =
    var index  = open
    var depth  = 1
    var result = Maybe.empty[Int]
    while result.isEmpty && index < text.length do
      val char = text.charAt(index)
      if char == '\\' then index += 2
      else if char == '`' then
        val run = backtickRun(text, index)
        closingRun(text, index + run, run) match
          case Present(closeStart) => index = closeStart + run
          case Absent              => index += run
      else if char == '<' && !skipMarkup(text, index).isEmpty then
        index = skipMarkup(text, index).getOrElse(index + 1)
      else
        if char == '[' then depth += 1
        else if char == ']' then
          depth -= 1
          if depth == 0 then result = Present(index)
        index += 1
    result

  /**
   * Parse `(destination "title")` starting just past the `(`.
   *
   * The destination is either angle-bracketed, which may hold spaces, or a bare run with balanced parentheses. The
   * title may be quoted with `"`, `'` or parentheses.
   */
  private def inlineTarget(text: String, from: Int): Maybe[Target] =
    var index = skipWhitespace(text, from)
    if index >= text.length then Absent
    else
      val destination: Maybe[(Int, String)] =
        if text.charAt(index) == '<' then
          angleDestinationEnd(text, index + 1) match
            case Absent         => Absent
            case Present(close) => Present((close + 1, unescape(text.substring(index + 1, close))))
        else
          var cursor = index
          var depth  = 0
          var broken = false
          while !broken && cursor < text.length do
            val char = text.charAt(cursor)
            if char == '\\' then cursor += 2
            else if char == '(' then { depth += 1; cursor += 1 }
            else if char == ')' then
              if depth == 0 then broken = true
              else { depth -= 1; cursor += 1 }
            else if char.isWhitespace then broken = true
            else cursor += 1
          Present((cursor, unescape(text.substring(index, cursor))))

      destination match
        case Absent                            => Absent
        case Present((afterDestination, dest)) =>
          index = skipWhitespace(text, afterDestination)
          if index < text.length && text.charAt(index) == ')' then
            Present((end = index + 1, destination = dest, title = Absent))
          else
            titleAt(text, index) match
              case Absent                       => Absent
              case Present((afterTitle, title)) =>
                val closing = skipWhitespace(text, afterTitle)
                if closing < text.length && text.charAt(closing) == ')' then
                  Present((end = closing + 1, destination = dest, title = Present(title)))
                else Absent

  /**
   * Where an angle-bracketed destination closes: the first `>` that no backslash escapes.
   *
   * A line ending or a bare `<` inside disqualifies it, and so does never closing -- and when it is disqualified the
   * link fails rather than falling back to the bare form, which is why `[link](<foo\>)` stays text with its escape
   * resolved.
   */
  private def angleDestinationEnd(text: String, open: Int): Maybe[Int] =
    @tailrec def scan(index: Int): Maybe[Int] =
      if index >= text.length then Absent
      else
        text.charAt(index) match
          case '\\'       => scan(index + 2)
          case '>'        => Present(index)
          case '<' | '\n' => Absent
          case _          => scan(index + 1)
    scan(open)

  private def titleAt(text: String, from: Int): Maybe[(Int, String)] =
    if from >= text.length then Absent
    else
      val opener = text.charAt(from)
      val closer = opener match
        case '"'  => '"'
        case '\'' => '\''
        case '('  => ')'
        case _    => '\u0000'
      if closer == '\u0000' then Absent
      else
        var cursor = from + 1
        var result = Maybe.empty[(Int, String)]
        while result.isEmpty && cursor < text.length do
          val char = text.charAt(cursor)
          if char == '\\' then cursor += 2
          else if char == closer then result = Present((cursor + 1, unescape(text.substring(from + 1, cursor))))
          else cursor += 1
        result

  /**
   * The destination and optional title of a link reference definition.
   *
   * Unlike an inline target there is no closing `)`; the definition ends at the end of the text, and anything after the
   * title other than whitespace disqualifies it.
   */
  private def definitionTargetImpl(text: String, from: Int): Maybe[Target] =
    val start = skipWhitespace(text, from)
    if start >= text.length then Absent
    else
      val destination: Maybe[(Int, String)] =
        if text.charAt(start) == '<' then
          val close = text.indexOf('>', start + 1)
          if close < 0 then Absent else Present((close + 1, unescape(text.substring(start + 1, close))))
        else
          var cursor = start
          while cursor < text.length && !text.charAt(cursor).isWhitespace do cursor += 1
          if cursor == start then Absent else Present((cursor, unescape(text.substring(start, cursor))))

      destination match
        case Absent                            => Absent
        case Present((afterDestination, dest)) =>
          val afterSpace = skipWhitespace(text, afterDestination)
          if afterSpace >= text.length || endsLine(text, afterDestination, afterSpace) && afterSpace >= text.length then
            Present((end = afterDestination, destination = dest, title = Absent))
          // Whitespace has to separate a title from the destination. Without that, `[foo]: <bar>(baz)` reads `(baz)`
          // as a parenthesised title and defines a link, where the spec leaves the whole line as prose.
          else if afterSpace == afterDestination then Absent
          else
            titleAt(text, afterSpace) match
              case Present((afterTitle, title)) if restIsBlank(text, afterTitle) =>
                Present((end = afterTitle, destination = dest, title = Present(title)))
              case _ =>
                // No title, so the definition ends at the destination -- but only if nothing else shares its line.
                if endsLine(text, afterDestination, afterSpace) then
                  Present((end = afterDestination, destination = dest, title = Absent))
                else Absent

  /** True when only whitespace separates `from` from the end of the text or the next line. */
  private def endsLine(text: String, from: Int, next: Int): Boolean =
    next >= text.length || text.substring(from, next).contains('\n')

  @tailrec private def restIsBlank(text: String, from: Int): Boolean =
    if from >= text.length then true
    else
      text.charAt(from) match
        case ' ' | '\t' => restIsBlank(text, from + 1)
        case '\n'       => true
        case _          => false

  @tailrec private def skipWhitespace(text: String, from: Int): Int =
    if from < text.length && text.charAt(from).isWhitespace then skipWhitespace(text, from + 1) else from

  /** Backslash escapes are live in destinations and titles, unlike inside a code span. */
  /**
   * What a destination or a title carries besides its own characters: backslash escapes and character references.
   *
   * Both are resolved here rather than left for the writer, because both end up in an attribute, where the writer
   * escapes on its own terms. `&quot;` in a title has to become a `"` before ScalaTags can write it back as `&quot;` --
   * passing it through would produce `&amp;quot;` instead -- and `&auml;` in a destination has to become `ä` before it
   * can be percent-encoded as `%C3%A4`.
   */
  private def unescape(value: String): String =
    val out   = StringBuilder()
    var index = 0
    while index < value.length do
      val char = value.charAt(index)
      if char == '\\' && index + 1 < value.length && isAsciiPunctuation(value.charAt(index + 1)) then
        out.append(value.charAt(index + 1))
        index += 2
      else if char == '&' then
        entityValue(value, index) match
          case Present((end, decoded)) =>
            out.append(decoded)
            index = end
          case Absent =>
            out.append(char)
            index += 1
      else
        out.append(char)
        index += 1
    out.toString

  /**
   * ASCII punctuation, which is what a backslash may escape.
   *
   * Only ASCII: `\€` is a backslash and a euro sign, not an escaped one, however much of a punctuation character the
   * flanking rules below consider `€` to be.
   */
  private def isAsciiPunctuation(char: Char): Boolean =
    "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~".indexOf(char.toInt) >= 0

  /**
   * Unicode whitespace, as the flanking rules mean it: a space separator, or one of the ASCII line and tab characters.
   *
   * Not `Char.isWhitespace`, which says no to a non-breaking space -- Java treats it as a space character but not as
   * whitespace. The spec disagrees, and example 353 is the difference: `*\u00a0a\u00a0*` is not emphasis, because the
   * runs are surrounded by whitespace and so flank neither way.
   */
  private def isUnicodeWhitespace(char: Char): Boolean =
    char == ' ' || char == '\t' || char == '\n' || char == '\r' || char == '\f' ||
      Character.getType(char) == Character.SPACE_SEPARATOR.toInt

  /**
   * Unicode punctuation, as the flanking rules mean it: any punctuation or symbol category.
   *
   * Symbols included, which is what example 354 turns on -- `*$*alpha` is not emphasis, and neither are the pound and
   * euro versions of it, because a currency symbol counts as punctuation here even though nothing would escape it.
   */
  private def isUnicodePunctuation(char: Char): Boolean =
    if char.toInt < 128 then isAsciiPunctuation(char)
    else
      Character.getType(char) match
        case Character.CONNECTOR_PUNCTUATION | Character.DASH_PUNCTUATION | Character.END_PUNCTUATION |
            Character.FINAL_QUOTE_PUNCTUATION | Character.INITIAL_QUOTE_PUNCTUATION | Character.OTHER_PUNCTUATION |
            Character.START_PUNCTUATION | Character.CURRENCY_SYMBOL | Character.MODIFIER_SYMBOL |
            Character.MATH_SYMBOL | Character.OTHER_SYMBOL =>
          true
        case _ => false

  /**
   * Percent-encode what a URI cannot carry literally.
   *
   * The safe set follows the reference implementation, which notably leaves `&` alone — the writer HTML-escapes it to
   * `&amp;` — and leaves an existing `%` alone rather than double-encoding it.
   */
  private def normalizeUri(value: String): String =
    val safe = "-_.+!*'(),%#@?=;:/&$~"
    val out  = StringBuilder()
    value.getBytes(java.nio.charset.StandardCharsets.UTF_8).foreach { byte =>
      val char = (byte & 0xff).toChar
      if char.isLetterOrDigit && char < 0x80 then out.append(char)
      else if safe.indexOf(char.toInt) >= 0 then out.append(char)
      else out.append("%%%02X".format(byte & 0xff))
    }
    out.toString

  /**
   * An autolink: `<` an absolute URI or an email address `>`.
   *
   * Returns where it ends, what it says, and where it points -- which differ for the email form, whose text is the
   * address and whose destination is that address behind `mailto:`.
   */
  private def autolink(text: String, start: Int): Maybe[Autolink] =
    val close = text.indexOf('>', start + 1)
    if close < 0 then Absent
    else
      val body = text.substring(start + 1, close)
      if isAbsoluteUri(body) then Present((end = close + 1, text = body, destination = body))
      else if isEmailAddress(body) then
        Present((end = close + 1, text = body, destination = s"mailto:$body"))
      else Absent

  /**
   * An absolute URI: a scheme, a `:`, then anything but whitespace and `<`.
   *
   * The scheme is two to thirty-two characters, and the lower bound is load-bearing: without it `<m:abc>` is a link,
   * where the spec leaves it as text.
   */
  private def isAbsoluteUri(body: String): Boolean =
    val schemeEnd = body.indexOf(':')
    schemeEnd >= 2 && schemeEnd <= 32 &&
    body.charAt(0).isLetter &&
    body.take(schemeEnd).forall(char => char.isLetterOrDigit || char == '+' || char == '.' || char == '-') &&
    !body.exists(char => char.isWhitespace || char == '<')

  /**
   * An email address, by the spec's own grammar rather than by anything stricter.
   *
   * Written out instead of expressed as the spec's regular expression, because the domain rule -- labels of letters,
   * digits and hyphens that neither begin nor end with a hyphen -- reads as three plain conditions and compiles the
   * same on every platform.
   */
  private def isEmailAddress(body: String): Boolean =
    val at = body.indexOf('@')
    at > 0 && at < body.length - 1 &&
    body.take(at).forall(isEmailLocalCharacter) &&
    body.drop(at + 1).split("\\.", -1).forall(isDomainLabel)

  private def isEmailLocalCharacter(char: Char): Boolean =
    char.isLetterOrDigit || ".!#$%&'*+/=?^_`{|}~-".indexOf(char.toInt) >= 0

  private def isDomainLabel(label: String): Boolean =
    label.nonEmpty && label.length <= 63 &&
      label.charAt(0).isLetterOrDigit && label.charAt(label.length - 1).isLetterOrDigit &&
      label.forall(char => char.isLetterOrDigit || char == '-')

  /**
   * Past an autolink or a piece of raw HTML beginning at `index`, if one does.
   *
   * Both bind tighter than a link label, so a `]` inside either is not the label's. Example 526 is the sharp case:
   * `[foo<https://example.com/?search=](uri)>` is an autolink whose URI happens to contain `](uri)`, and the label
   * never closes.
   */
  private def skipMarkup(text: String, index: Int): Maybe[Int] =
    autolink(text, index) match
      case Present(link) => Present(link.end)
      case Absent        => HtmlTag.endOf(text, index)

  /**
   * Whether a node is a link or holds one at any depth.
   *
   * Emphasis is transparent to the rule, which is what example 519 turns on: a link inside emphasis inside a label
   * still stops the label becoming a link. An image's alt text is a `String` by then, so a link inside one has already
   * flattened and cannot be found -- which is right, because an image may hold a link.
   */
  private def holdsLink(node: Inline): Boolean = node match
    case Inline.Link(_, _, _, _)         => true
    case Inline.Emphasis(inner, _)       => inner.exists(holdsLink)
    case Inline.StrongEmphasis(inner, _) => inner.exists(holdsLink)
    case _                               => false

  /** The plain text of a label, which is what an `alt` attribute can hold. */
  private def plainText(label: String, definitions: Map[String, LinkDefinition]): String =
    parse(label, identity, definitions).map(plainOf).mkString

  /** Flatten a node to the text an attribute can carry: markup contributes its content, not its markers. */
  private def plainOf(node: Inline): String = node match
    case Inline.Text(value, _)           => value
    case Inline.CodeSpan(value, _)       => value
    case Inline.Link(_, _, inner, _)     => inner.map(plainOf).mkString
    case Inline.Image(_, _, alt, _)      => alt
    case Inline.Emphasis(inner, _)       => inner.map(plainOf).mkString
    case Inline.StrongEmphasis(inner, _) => inner.map(plainOf).mkString
    // An `alt` attribute is text, and raw HTML in a label contributes none: it is markup, not content.
    case Inline.RawHtml(_, _) => ""
    // A break in alt text is the line ending it stands for; the `alt` attribute has no markup to carry it.
    case Inline.LineBreak(_) => "\n"

  private def spanOf(start: Int, end: Int, sourceOffsetAt: Int => Int): Span =
    Span.fromStartEnd(sourceOffsetAt(start), sourceOffsetAt(end))

  /** The length of the backtick run beginning at `start`. */
  private def backtickRun(text: String, start: Int): Int =
    @tailrec def run(end: Int): Int =
      if end < text.length && text.charAt(end) == '`' then run(end + 1) else end - start
    run(start)

  /**
   * Where the closing backtick run of exactly `length` begins, searching from `from`.
   *
   * A run of a different length cannot close the span and is skipped whole, which is what makes ``` ``foo`bar`` ``` one
   * span rather than two.
   */
  private def closingRun(text: String, from: Int, length: Int): Maybe[Int] =
    @tailrec def search(index: Int): Maybe[Int] =
      if index >= text.length then Absent
      else if text.charAt(index) == '`' then
        val run = backtickRun(text, index)
        // A run of a different length cannot close the span, so it is skipped whole rather than re-entered.
        if run == length then Present(index) else search(index + run)
      else search(index + 1)
    search(from)

  /**
   * CommonMark's code-span content rules.
   *
   * Line endings become spaces. Then, if the result begins and ends with a space but is not all spaces, one space is
   * removed from each end — one, not all, so `` `  `` ` `` keeps the inner pair. "Space" here means U+0020 only, so a
   * non-breaking space is content and never stripped.
   */
  private def normalize(content: String): String =
    val spaced = content.replace("\r\n", " ").replace('\n', ' ').replace('\r', ' ')
    if spaced.length >= 2 && spaced.startsWith(" ") && spaced.endsWith(" ") && spaced.exists(_ != ' ') then
      spaced.substring(1, spaced.length - 1)
    else spaced
end InlineParser
