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
private[markdown] object InlineParser:

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
  def parse(text: String, sourceOffsetAt: Int => Int): Chunk[Inline] =
    val nodes        = List.newBuilder[Inline]
    val pending      = StringBuilder()
    var pendingStart = 0
    var index        = 0

    def flushPending(): Unit =
      if pending.nonEmpty then
        val value = pending.toString
        nodes += Inline.Text(value, spanOf(pendingStart, index, sourceOffsetAt))
        pending.clear()

    while index < text.length do
      // Code spans, autolinks and links each claim a run; anything unclaimed accumulates as text.
      constructAt(text, index, sourceOffsetAt) match
        case Present((end, node)) =>
          flushPending()
          nodes += node
          index = end
          pendingStart = index
        case Absent =>
          val run =
            if text.charAt(index) == '`' then backtickRun(text, index)
            else 1
          if pending.isEmpty then pendingStart = index
          pending.append(text.substring(index, index + run))
          index += run
    end while
    flushPending()
    Chunk.from(nodes.result())
  end parse

  /** The inline construct beginning at `index`, if any, and the index just past it. */
  private def constructAt(text: String, index: Int, sourceOffsetAt: Int => Int): Maybe[(Int, Inline)] =
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
      linkAt(text, index, open, image, sourceOffsetAt)
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
      sourceOffsetAt: Int => Int
  ): Maybe[(Int, Inline)] =
    labelEnd(text, open) match
      case Absent         => Absent
      case Present(close) =>
        if close + 1 >= text.length || text.charAt(close + 1) != '(' then Absent
        else
          inlineTarget(text, close + 2) match
            case Absent                             => Absent
            case Present((end, destination, title)) =>
              val span  = spanOf(start, end, sourceOffsetAt)
              val label = text.substring(open, close)
              val node  =
                if image then Inline.Image(normalizeUri(destination), title, plainText(label), span)
                else
                  val content = parse(label, index => sourceOffsetAt(open + index))
                  Inline.Link(normalizeUri(destination), title, content, span)
              Present((end, node))

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
  private def plainText(label: String): String =
    parse(label, identity).map {
      case Inline.Text(value, _)       => value
      case Inline.CodeSpan(value, _)   => value
      case Inline.Link(_, _, inner, _) => inner.map(plainOf).mkString
      case Inline.Image(_, _, alt, _)  => alt
    }.mkString

  private def plainOf(node: Inline): String = node match
    case Inline.Text(value, _)       => value
    case Inline.CodeSpan(value, _)   => value
    case Inline.Link(_, _, inner, _) => inner.map(plainOf).mkString
    case Inline.Image(_, _, alt, _)  => alt

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
