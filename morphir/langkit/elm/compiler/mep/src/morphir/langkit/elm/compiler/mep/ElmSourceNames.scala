package morphir.langkit.elm.compiler.mep

import scala.annotation.tailrec

/**
 * The module an Elm source names, read lexically from its header without parsing the source.
 *
 * Workspace discovery names a source before any compile, and it must name it the same way as the other Morphir Elm
 * providers do: this is a port of the header scanner in finos/morphir-rust (`morphir-elm-binding`) and
 * finos/morphir-elm (`cli2/mep/elm-names.ts`). A real parser would reject sources those scanners name, so it is not
 * used here.
 */
private[mep] object ElmSourceNames:
  /** The module a source declares in a `module`, `port module` or `effect module` header, if it has one. */
  def declaredModuleName(source: String): Option[String] =
    for
      start         <- skipTrivia(source, 0)
      (kind, after) <- declarationKind(source, start)
      moduleEnd     <- keywordEnd(source, after, "module")
      pathStart     <- skipTrivia(source, moduleEnd)
      (name, end)   <- modulePath(source, pathStart)
      suffixStart   <- skipTrivia(source, end)
      _             <- keywordEnd(source, suffixStart, if kind == "effect" then "where" else "exposing")
    yield name

  /** The module a source without a declared header defines: its file stem when that is a module path, else `Main`. */
  def fallbackModuleName(fileName: String): String =
    val extension = fileName.lastIndexOf('.')
    val stem      = if extension > 0 then fileName.substring(0, extension) else fileName
    modulePath(stem, 0) match
      case Some((name, end)) if end == stem.length => name
      case _                                       => "Main"

  /** The module a selected source defines: its declared header, then its file stem, then `Main`. */
  def moduleName(fileName: String, source: String): String =
    declaredModuleName(source).getOrElse(fallbackModuleName(fileName))

  private def declarationKind(source: String, offset: Int): Option[(String, Int)] =
    keywordEnd(source, offset, "port").map("port" -> _)
      .orElse(keywordEnd(source, offset, "effect").map("effect" -> _)) match
      case Some((kind, end)) => skipTrivia(source, end).map(kind -> _)
      case None              => Some("normal" -> offset)

  private def isIdentifierCharacter(source: String, offset: Int): Boolean =
    offset < source.length && {
      val character = source.charAt(offset)
      isAsciiUpper(character) || (character >= 'a' && character <= 'z') ||
      (character >= '0' && character <= '9') || character == '_'
    }

  private def isWhitespace(character: Char): Boolean =
    character == '\uFEFF' || character == '\t' || character == '\n' || character == '\f' || character == '\r' ||
      character == ' '

  /** The offset after whitespace and comments, or `None` when a block comment is not closed. */
  @tailrec
  private def skipTrivia(source: String, offset: Int): Option[Int] =
    if offset >= source.length then Some(offset)
    else if isWhitespace(source.charAt(offset)) then skipTrivia(source, offset + 1)
    else if source.startsWith("--", offset) then
      val lineEnd = source.indexOf('\n', offset + 2)
      skipTrivia(source, if lineEnd == -1 then source.length else lineEnd + 1)
    else if source.startsWith("{-", offset) then
      blockCommentEnd(source, offset + 2, depth = 1) match
        case Some(end) => skipTrivia(source, end)
        case None      => None
    else Some(offset)

  /** The offset after the block comment open to `depth` at `offset`; block comments nest. */
  @tailrec
  private def blockCommentEnd(source: String, offset: Int, depth: Int): Option[Int] =
    if depth == 0 then Some(offset)
    else if offset >= source.length then None
    else if source.startsWith("{-", offset) then blockCommentEnd(source, offset + 2, depth + 1)
    else if source.startsWith("-}", offset) then blockCommentEnd(source, offset + 2, depth - 1)
    else blockCommentEnd(source, offset + 1, depth)

  private def keywordEnd(source: String, offset: Int, keyword: String): Option[Int] =
    val end = offset + keyword.length
    Option.when(source.startsWith(keyword, offset) && !isIdentifierCharacter(source, end))(end)

  /** A dotted module path of capitalised segments starting at `start`, and the offset after it. */
  private def modulePath(source: String, start: Int): Option[(String, Int)] =
    @tailrec
    def segments(offset: Int, names: Vector[String]): Option[(String, Int)] =
      if offset >= source.length || !isAsciiUpper(source.charAt(offset)) then None
      else
        val end   = identifierEnd(source, offset + 1)
        val found = names :+ source.substring(offset, end)
        if end < source.length && source.charAt(end) == '.' then segments(end + 1, found)
        else Some(found.mkString(".") -> end)
    segments(start, Vector.empty)

  @tailrec
  private def identifierEnd(source: String, offset: Int): Int =
    if isIdentifierCharacter(source, offset) then identifierEnd(source, offset + 1) else offset

  private def isAsciiUpper(character: Char): Boolean = character >= 'A' && character <= 'Z'
end ElmSourceNames
