package morphir.langkit.markdown.internal

import kyo.*
import scala.annotation.tailrec

/**
 * Where a piece of raw HTML ends, if one begins where you are looking.
 *
 * "Valid" is load-bearing throughout, and is the whole reason this exists rather than a search for the next `>`. A `<`
 * that does not open something the spec recognises is an ordinary character: `<a h*#ref="hi">` has no legal attribute
 * name, `<a href='bar'title=title>` has attributes that do not separate, and `</a href="foo">` is a closing tag with
 * attributes -- none of them is HTML, and all three stay escaped text.
 *
 * Shared by the block parser, which asks whether a whole line is one tag, and the inline parser, which asks whether raw
 * HTML begins at some index in a paragraph. They differ only in which forms they accept and in what they do with the
 * answer, so the scanning lives here once.
 */
private[markdown] object HtmlTag:

  /**
   * Where an open or closing tag beginning at `from` ends.
   *
   * The block parser's condition seven, which recognises tags and nothing else: a comment or a declaration alone on a
   * line opens a block through its own condition, with its own way of ending.
   */
  def tagEndOf(text: String, from: Int): Maybe[Int] =
    if from >= text.length || text.charAt(from) != '<' then Absent
    else if from + 1 < text.length && text.charAt(from + 1) == '/' then closingTagEnd(text, from)
    else openTagEnd(text, from)

  /**
   * Where any raw HTML beginning at `from` ends: a tag, a comment, a processing instruction, a declaration or CDATA.
   *
   * All six forms of the inline construct. Order matters at the top: `<!--` has to be tried as a comment before `<!` is
   * tried as a declaration, and `<![CDATA[` before either.
   */
  def endOf(text: String, from: Int): Maybe[Int] =
    if from >= text.length || text.charAt(from) != '<' then Absent
    else if text.startsWith("<!--", from) then commentEnd(text, from)
    else if text.startsWith("<![CDATA[", from) then delimitedEnd(text, from + 9, "]]>")
    else if text.startsWith("<?", from) then delimitedEnd(text, from + 2, "?>")
    else if text.startsWith("<!", from) then declarationEnd(text, from)
    else tagEndOf(text, from)

  /** Where a tag's name ends: letters, digits and hyphens. */
  @tailrec def nameEnd(text: String, from: Int): Int =
    if from < text.length && (text.charAt(from).isLetterOrDigit || text.charAt(from) == '-') then
      nameEnd(text, from + 1)
    else from

  /**
   * A comment: `<!-->`, `<!--->`, or `<!--` then anything without `-->` then `-->`.
   *
   * The two short forms are spelled out because the general one cannot reach them -- it needs seven characters and they
   * are five and six. `--` inside a comment is fine; only `-->` closes it, which is why example 625 is one comment
   * across two lines and example 626 is `<!-->` followed by ordinary text.
   */
  private def commentEnd(text: String, from: Int): Maybe[Int] =
    if text.startsWith("<!-->", from) then Present(from + 5)
    else if text.startsWith("<!--->", from) then Present(from + 6)
    else delimitedEnd(text, from + 4, "-->")

  /** A declaration: `<!`, an ASCII letter, then anything up to the first `>`. */
  private def declarationEnd(text: String, from: Int): Maybe[Int] =
    if from + 2 >= text.length || !text.charAt(from + 2).isLetter then Absent
    else delimitedEnd(text, from + 3, ">")

  /** The first `terminator` at or after `from`, counting past it. */
  private def delimitedEnd(text: String, from: Int, terminator: String): Maybe[Int] =
    val close = text.indexOf(terminator, from)
    if close < 0 then Absent else Present(close + terminator.length)

  private def closingTagEnd(text: String, from: Int): Maybe[Int] =
    val start = from + 2
    if start >= text.length || !text.charAt(start).isLetter then Absent
    else
      val afterSpaces = skipWhitespace(text, nameEnd(text, start))
      // A closing tag takes no attributes: anything but whitespace before the `>` disqualifies it.
      if afterSpaces < text.length && text.charAt(afterSpaces) == '>' then Present(afterSpaces + 1) else Absent

  private def openTagEnd(text: String, from: Int): Maybe[Int] =
    if from + 1 >= text.length || !text.charAt(from + 1).isLetter then Absent
    else
      @tailrec def attributes(index: Int): Maybe[Int] =
        val afterSpaces = skipWhitespace(text, index)
        if afterSpaces >= text.length then Absent
        else if text.charAt(afterSpaces) == '>' then Present(afterSpaces + 1)
        else if text.charAt(afterSpaces) == '/' && afterSpaces + 1 < text.length &&
          text.charAt(afterSpaces + 1) == '>'
        then Present(afterSpaces + 2)
        else if afterSpaces == index then Absent // attributes must be separated by whitespace
        else
          attributeEnd(text, afterSpaces) match
            case Present(next) => attributes(next)
            case Absent        => Absent
      attributes(nameEnd(text, from + 1))

  private def attributeEnd(text: String, from: Int): Maybe[Int] =
    if from >= text.length then Absent
    else if !(text.charAt(from).isLetter || text.charAt(from) == '_' || text.charAt(from) == ':') then Absent
    else
      @tailrec def nameRun(index: Int): Int =
        if index < text.length &&
          (text.charAt(index).isLetterOrDigit || "_.:-".indexOf(text.charAt(index).toInt) >= 0)
        then nameRun(index + 1)
        else index

      val end       = nameRun(from)
      val afterName = skipWhitespace(text, end)
      if afterName >= text.length || text.charAt(afterName) != '=' then Present(end)
      else
        val valueStart = skipWhitespace(text, afterName + 1)
        if valueStart >= text.length then Absent
        else
          val quote = text.charAt(valueStart)
          if quote == '"' || quote == '\'' then
            val close = text.indexOf(quote.toInt, valueStart + 1)
            if close < 0 then Absent else Present(close + 1)
          else
            @tailrec def unquotedEnd(cursor: Int): Int =
              if cursor < text.length && !text.charAt(cursor).isWhitespace &&
                "\"'=<>`".indexOf(text.charAt(cursor).toInt) < 0
              then unquotedEnd(cursor + 1)
              else cursor
            val cursor = unquotedEnd(valueStart)
            if cursor == valueStart then Absent else Present(cursor)

  @tailrec private def skipWhitespace(text: String, from: Int): Int =
    if from < text.length && text.charAt(from).isWhitespace then skipWhitespace(text, from + 1) else from
end HtmlTag
