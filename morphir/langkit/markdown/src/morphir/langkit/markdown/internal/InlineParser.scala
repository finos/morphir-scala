package morphir.langkit.markdown.internal

import kyo.*
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

  /** Where a label opened at `open` closes; used by block parsing to find a definition's label. */
  def labelEndOf(text: String, open: Int): Maybe[Int] = labelEnd(text, open)

  /**
   * A definition's `destination "title"` tail, which must be followed by nothing but whitespace.
   *
   * Stricter than an inline link's target: example 209 shows `[foo]: /url "title" ok` is not a definition at all,
   * because trailing content disqualifies it.
   */
  def definitionTarget(text: String, from: Int): Maybe[(Int, String, Maybe[String])] =
    definitionTargetImpl(text, from)

  /** Normalise a link label the way the spec matches them: trimmed, whitespace collapsed, case folded. */
  def normalizeLabel(label: String): String =
    label.trim.replaceAll("\\s+", " ").toLowerCase

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
      if char == '\\' && index + 1 < text.length && isPunctuation(text.charAt(index + 1)) then
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

    val beforeWhitespace = before.isWhitespace
    val afterWhitespace  = after.isWhitespace
    val beforePunct      = isPunctuation(before)
    val afterPunct       = isPunctuation(after)

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
    if char == '`' then
      val run = backtickRun(text, index)
      closingRun(text, index + run, run).map { closeStart =>
        val end = closeStart + run
        (end, Inline.CodeSpan(normalize(text.substring(index + run, closeStart)), spanOf(index, end, sourceOffsetAt)))
      }
    else if char == '<' then
      autolink(text, index).map { case (end, uri) =>
        val span = spanOf(index, end, sourceOffsetAt)
        (end, Inline.Link(normalizeUri(uri), Absent, Chunk(Inline.Text(uri, span)), span))
      }
    else if isLinkStart(text, index) then
      val image = char == '!'
      val open  = if image then index + 2 else index + 1
      linkAt(text, index, open, image, sourceOffsetAt, definitions)
    else Absent

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

        def build(end: Int, destination: String, title: Maybe[String], normalize: Boolean): (Int, Inline) =
          val span = spanOf(start, end, sourceOffsetAt)
          val uri  = if normalize then normalizeUri(destination) else destination
          val node =
            if image then Inline.Image(uri, title, plainText(label, definitions), span)
            else Inline.Link(uri, title, parse(label, index => sourceOffsetAt(open + index), definitions), span)
          (end, node)

        val inlineForm =
          if close + 1 < text.length && text.charAt(close + 1) == '(' then
            inlineTarget(text, close + 2).map { case (end, destination, title) =>
              build(end, destination, title, true)
            }
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
      build: (Int, String, Maybe[String], Boolean) => (Int, Inline)
  ): Maybe[(Int, Inline)] =
    val explicit =
      if close + 1 < text.length && text.charAt(close + 1) == '[' then labelEnd(text, close + 2)
      else Absent

    val (referenceLabel, end) = explicit match
      case Present(secondClose) =>
        val named = text.substring(close + 2, secondClose)
        ((if named.isBlank then label else named), secondClose + 1)
      case Absent => (label, close + 1)

    definitions.get(normalizeLabel(referenceLabel)) match
      case Some(definition) => Present(build(end, definition.destination, definition.title, false))
      case None             => Absent

  /**
   * Where the label that opened at `open` closes.
   *
   * Brackets nest, so `[link [foo [bar]]]` closes at the last one. A code span binds tighter than a link, so a bracket
   * inside one is skipped rather than counted — which is why `[not a `link](/foo`)` is a code span and not a link.
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
  private def inlineTarget(text: String, from: Int): Maybe[(Int, String, Maybe[String])] =
    var index = skipWhitespace(text, from)
    if index >= text.length then Absent
    else
      val destination: Maybe[(Int, String)] =
        if text.charAt(index) == '<' then
          val close = text.indexOf('>', index + 1)
          if close < 0 then Absent else Present((close + 1, unescape(text.substring(index + 1, close))))
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
          if index < text.length && text.charAt(index) == ')' then Present((index + 1, dest, Absent))
          else
            titleAt(text, index) match
              case Absent                       => Absent
              case Present((afterTitle, title)) =>
                val closing = skipWhitespace(text, afterTitle)
                if closing < text.length && text.charAt(closing) == ')' then
                  Present((closing + 1, dest, Present(title)))
                else Absent

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
  private def definitionTargetImpl(text: String, from: Int): Maybe[(Int, String, Maybe[String])] =
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
            Present((afterDestination, dest, Absent))
          else
            titleAt(text, afterSpace) match
              case Present((afterTitle, title)) if restIsBlank(text, afterTitle) =>
                Present((afterTitle, dest, Present(title)))
              case _ =>
                // No title, so the definition ends at the destination -- but only if nothing else shares its line.
                if endsLine(text, afterDestination, afterSpace) then Present((afterDestination, dest, Absent))
                else Absent

  /** True when only whitespace separates `from` from the end of the text or the next line. */
  private def endsLine(text: String, from: Int, next: Int): Boolean =
    next >= text.length || text.substring(from, next).contains('\n')

  private def restIsBlank(text: String, from: Int): Boolean =
    var index = from
    while index < text.length && (text.charAt(index) == ' ' || text.charAt(index) == '\t') do index += 1
    index >= text.length || text.charAt(index) == '\n'

  private def skipWhitespace(text: String, from: Int): Int =
    var index = from
    while index < text.length && text.charAt(index).isWhitespace do index += 1
    index

  /** Backslash escapes are live in destinations and titles, unlike inside a code span. */
  private def unescape(value: String): String =
    val out   = StringBuilder()
    var index = 0
    while index < value.length do
      val char = value.charAt(index)
      if char == '\\' && index + 1 < value.length && isPunctuation(value.charAt(index + 1)) then
        out.append(value.charAt(index + 1))
        index += 2
      else
        out.append(char)
        index += 1
    out.toString

  private def isPunctuation(char: Char): Boolean = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~".indexOf(char.toInt) >= 0

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
   * Requires a scheme followed by `:` and no spaces or `<`, which is what keeps `<not a link>` ordinary text.
   */
  private def autolink(text: String, start: Int): Maybe[(Int, String)] =
    val close = text.indexOf('>', start + 1)
    if close < 0 then Absent
    else
      val body        = text.substring(start + 1, close)
      val schemeEnd   = body.indexOf(':')
      val validScheme =
        schemeEnd > 0 &&
          body.charAt(0).isLetter &&
          body.take(schemeEnd).forall(c => c.isLetterOrDigit || c == '+' || c == '.' || c == '-')
      if validScheme && !body.exists(c => c.isWhitespace || c == '<') then Present((close + 1, body)) else Absent

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

  private def spanOf(start: Int, end: Int, sourceOffsetAt: Int => Int): Span =
    Span.fromStartEnd(sourceOffsetAt(start), sourceOffsetAt(end))

  /** The length of the backtick run beginning at `start`. */
  private def backtickRun(text: String, start: Int): Int =
    var end = start
    while end < text.length && text.charAt(end) == '`' do end += 1
    end - start

  /**
   * Where the closing backtick run of exactly `length` begins, searching from `from`.
   *
   * A run of a different length cannot close the span and is skipped whole, which is what makes ``` ``foo`bar`` ``` one
   * span rather than two.
   */
  private def closingRun(text: String, from: Int, length: Int): Maybe[Int] =
    var index  = from
    var result = Maybe.empty[Int]
    while result.isEmpty && index < text.length do
      if text.charAt(index) == '`' then
        val run = backtickRun(text, index)
        if run == length then result = Present(index)
        else index += run
      else index += 1
    result

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
