package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.scanner.*

/**
 * Structured view of a CommonMark fenced-code info string.
 *
 * CommonMark keeps the info string opaque. Language, CLI-style tokens, and Pandoc brace attributes are conventions
 * derived from [[raw]]. Construct only through [[FenceInfo.parse]] or [[FenceInfo.empty]] so fields stay consistent
 * with that string.
 */
sealed abstract case class FenceInfo private[markdown] (
    raw: String,
    language: Maybe[String],
    args: Chunk[String],
    id: Maybe[String],
    classes: Chunk[String],
    attributes: Chunk[(key: String, value: String)],
    flags: Chunk[String]
) derives CanEqual

object FenceInfo:

  /** Covers token text, token-list linkage, and derived metadata collection retention. */
  private[markdown] val TokenOutputReservation: NodeCount = NodeCount(8L)

  private val WorkUnitsPerCodeUnit = 8L

  val empty: FenceInfo =
    make(
      raw = "",
      language = Absent,
      args = Chunk.empty,
      id = Absent,
      classes = Chunk.empty,
      attributes = Chunk.empty,
      flags = Chunk.empty
    )

  /** Derive structured fields from a CommonMark info string. Never fails the fence. */
  def parse(raw: String): FenceInfo =
    parseWithReservation(raw)(() => ())

  private[markdown] def parseBudgeted(raw: String, scanner: SourceScanner): FenceInfo =
    scanner.chargeWork(WorkUnits(raw.length.toLong * WorkUnitsPerCodeUnit))
    parseWithReservation(raw)(() => scanner.chargeOutputNodes(TokenOutputReservation))

  private def parseWithReservation(raw: String)(reserveToken: () => Unit): FenceInfo =
    val trimmed = trimSpacesOrTabs(raw)
    if trimmed.isEmpty then empty
    else if trimmed.charAt(0) == '{' then parseBraceLed(trimmed, reserveToken)
    else parseBareLed(trimmed, reserveToken)

  extension (self: FenceInfo)
    def tokens: Chunk[String] =
      self.language match
        case Present(lang) => Chunk(lang).concat(self.args)
        case Absent        => self.args

    def flag(name: String): Boolean =
      self.flags.contains(name)

    def option(key: String): Maybe[String] =
      self.attributes.find(_.key == key) match
        case Some(attr) => Present(attr.value)
        case None       => Absent

  private def make(
      raw: String,
      language: Maybe[String],
      args: Chunk[String],
      id: Maybe[String],
      classes: Chunk[String],
      attributes: Chunk[(key: String, value: String)],
      flags: Chunk[String]
  ): FenceInfo =
    new FenceInfo(raw, language, args, id, classes, attributes, flags) {}

  private def parseBraceLed(raw: String, reserveToken: () => Unit): FenceInfo =
    extractBalancedBrace(raw) match
      case Present((contents = contents, rest = rest)) if isSpacesOrTabs(rest) =>
        val attrs = parsePandocAttrs(contents, promoteFirstClass = true, reserveToken)
        make(
          raw = raw,
          language = attrs.language,
          args = Chunk.empty,
          id = attrs.id,
          classes = attrs.classes,
          attributes = attrs.attributes,
          flags = Chunk.empty
        )
      case _ =>
        make(
          raw = raw,
          language = Absent,
          args = Chunk.empty,
          id = Absent,
          classes = Chunk.empty,
          attributes = Chunk.empty,
          flags = Chunk.empty
        )

  private def parseBareLed(raw: String, reserveToken: () => Unit): FenceInfo =
    splitTrailingBrace(raw) match
      case (cli = cli, brace = brace) =>
        val cliTokens = splitTokens(cli, reserveToken)
        if cliTokens.isEmpty then
          brace match
            case Present(contents) =>
              val attrs = parsePandocAttrs(contents, promoteFirstClass = true, reserveToken)
              make(
                raw = raw,
                language = attrs.language,
                args = Chunk.empty,
                id = attrs.id,
                classes = attrs.classes,
                attributes = attrs.attributes,
                flags = Chunk.empty
              )
            case Absent =>
              make(
                raw = raw,
                language = Absent,
                args = Chunk.empty,
                id = Absent,
                classes = Chunk.empty,
                attributes = Chunk.empty,
                flags = Chunk.empty
              )
        else
          val language     = Present(cliTokens.head)
          val rest         = cliTokens.tail
          val args         = Chunk.from(rest)
          val flagsBuilder = List.newBuilder[String]
          val attrBuilder  = List.newBuilder[(key: String, value: String)]
          rest.foreach { token =>
            splitOption(token) match
              case Present(attr) => attrBuilder += attr
              case Absent        => flagsBuilder += token
          }
          val braceAttrs = brace match
            case Present(contents) => parsePandocAttrs(contents, promoteFirstClass = false, reserveToken)
            case Absent            => PandocAttrs.empty
          make(
            raw = raw,
            language = language,
            args = args,
            id = braceAttrs.id,
            classes = braceAttrs.classes,
            attributes = Chunk.from(attrBuilder.result()).concat(braceAttrs.attributes),
            flags = Chunk.from(flagsBuilder.result())
          )

  private final case class PandocAttrs(
      language: Maybe[String],
      id: Maybe[String],
      classes: Chunk[String],
      attributes: Chunk[(key: String, value: String)]
  )

  private object PandocAttrs:
    val empty: PandocAttrs =
      PandocAttrs(Absent, Absent, Chunk.empty, Chunk.empty)

  /**
   * @param promoteFirstClass
   *   when true (brace-led info), the first class becomes [[FenceInfo.language]] and is omitted from
   *   [[FenceInfo.classes]]; when false (trailing braces after a bare language), every class stays in
   *   [[FenceInfo.classes]].
   */
  private def parsePandocAttrs(
      contents: String,
      promoteFirstClass: Boolean,
      reserveToken: () => Unit
  ): PandocAttrs =
    val tokens   = splitTokens(contents, reserveToken)
    var id       = Option.empty[String]
    val classBuf = List.newBuilder[String]
    val attrBuf  = List.newBuilder[(key: String, value: String)]
    tokens.foreach { token =>
      if token.startsWith("#") && token.length > 1 then id = Some(token.drop(1))
      else if token.startsWith(".") && token.length > 1 then classBuf += token.drop(1)
      else
        splitOption(token) match
          case Present(attr) => attrBuf += attr
          case Absent        => classBuf += token
    }
    val classes                      = classBuf.result()
    val (language, remainingClasses) =
      if promoteFirstClass then
        (
          classes.headOption match
            case Some(lang) => Present(lang)
            case None       =>
              Absent
          ,
          if classes.isEmpty then Chunk.empty else Chunk.from(classes.tail)
        )
      else (Absent, Chunk.from(classes))
    PandocAttrs(
      language = language,
      id = id match
        case Some(value) => Present(value)
        case None        => Absent
      ,
      classes = remainingClasses,
      attributes = Chunk.from(attrBuf.result())
    )

  /** Split a trailing `{...}` from CLI text. Malformed braces leave the whole string as CLI. */
  private def splitTrailingBrace(raw: String): (cli: String, brace: Maybe[String]) =
    val start = raw.indexOf('{')
    if start < 0 then (cli = raw, brace = Absent)
    else
      extractBalancedBrace(raw.substring(start)) match
        case Present((contents = contents, rest = rest)) if isSpacesOrTabs(rest) =>
          (cli = trimSpacesOrTabs(raw.substring(0, start)), brace = Present(contents))
        case _ =>
          (cli = raw, brace = Absent)

  private def extractBalancedBrace(text: String): Maybe[(contents: String, rest: String)] =
    if text.isEmpty || text.charAt(0) != '{' then Absent
    else
      var depth = 0
      var i     = 0
      var done  = false
      var quote = Option.empty[Char]
      while i < text.length && !done do
        val ch = text.charAt(i)
        quote match
          case Some(q) =>
            if ch == '\\' && i + 1 < text.length then i += 2
            else
              if ch == q then quote = None
              i += 1
          case None =>
            ch match
              case '"' | '\'' =>
                quote = Some(ch)
                i += 1
              case '{' =>
                depth += 1
                i += 1
              case '}' =>
                depth -= 1
                i += 1
                if depth == 0 then done = true
              case _ => i += 1
      if depth == 0 && done then Present((contents = text.substring(1, i - 1), rest = text.substring(i)))
      else Absent

  private def splitOption(token: String): Maybe[(key: String, value: String)] =
    val eq = token.indexOf('=')
    if eq <= 0 then Absent
    else
      val key   = token.substring(0, eq)
      val value = unquote(token.substring(eq + 1))
      Present((key = key, value = value))

  private def unquote(value: String): String =
    if value.length >= 2 then
      val first = value.charAt(0)
      val last  = value.charAt(value.length - 1)
      if (first == '"' && last == '"') || (first == '\'' && last == '\'') then value.substring(1, value.length - 1)
      else value
    else value

  private def splitTokens(text: String, reserveToken: () => Unit): List[String] =
    val result = List.newBuilder[String]
    var i      = 0
    while i < text.length do
      while i < text.length && isSpaceOrTab(text.charAt(i)) do i += 1
      if i < text.length then
        val start = i
        val first = text.charAt(i)
        if first == '"' || first == '\'' then
          i = skipQuoted(text, i)
          reserveToken()
          result += text.substring(start, i)
        else
          while i < text.length && !isSpaceOrTab(text.charAt(i)) do
            if text.charAt(i) == '=' && i + 1 < text.length && isQuote(text.charAt(i + 1)) then
              i = skipQuoted(text, i + 1)
            else i += 1
          reserveToken()
          result += text.substring(start, i)
    result.result()

  private def isQuote(char: Char): Boolean =
    char == '"' || char == '\''

  /** Advance past a quoted span starting at `quoteIndex` (the opening quote). Leaves `i` after the closer. */
  private def skipQuoted(text: String, quoteIndex: Int): Int =
    val quote = text.charAt(quoteIndex)
    var i     = quoteIndex + 1
    while i < text.length && text.charAt(i) != quote do
      if text.charAt(i) == '\\' && i + 1 < text.length then i += 2
      else i += 1
    if i < text.length then i + 1 else i

  private def isSpaceOrTab(char: Char): Boolean =
    char == ' ' || char == '\t'

  private def isSpacesOrTabs(text: String): Boolean =
    text.forall(isSpaceOrTab)

  private def trimSpacesOrTabs(text: String): String =
    val start = text.indexWhere(char => !isSpaceOrTab(char))
    if start == -1 then ""
    else
      val end = text.lastIndexWhere(char => !isSpaceOrTab(char))
      text.substring(start, end + 1)
