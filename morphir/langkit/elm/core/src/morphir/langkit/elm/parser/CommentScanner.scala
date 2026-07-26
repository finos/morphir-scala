package morphir.langkit.elm.parser

import scala.collection.mutable.ListBuffer

import morphir.langkit.elm.Span
import morphir.langkit.elm.cst.{CommentKind, CstComment}

object CommentScanner:

  def scan(source: String): List[CstComment] =
    val comments = ListBuffer.empty[CstComment]
    var offset   = 0

    while offset < source.length do
      if startsWith(source, offset, "--") then
        val start = offset
        offset += 2
        val textStart = offset
        while offset < source.length && source.charAt(offset) != '\n' && source.charAt(offset) != '\r' do
          offset += 1
        comments += CstComment(CommentKind.Line, source.substring(textStart, offset))(
          Span(start, offset - start)
        )
      else if startsWith(source, offset, "{-|") then
        val start = offset
        offset += 3
        val textStart = offset
        offset = scanBlockCommentEnd(source, offset)
        val textEnd = (offset - 2).max(textStart)
        comments += CstComment(CommentKind.Doc, source.substring(textStart, textEnd))(
          Span(start, offset - start)
        )
      else if startsWith(source, offset, "{-") then
        val start = offset
        offset += 2
        val textStart = offset
        offset = scanBlockCommentEnd(source, offset)
        val textEnd = (offset - 2).max(textStart)
        comments += CstComment(CommentKind.Block, source.substring(textStart, textEnd))(
          Span(start, offset - start)
        )
      else if source.charAt(offset) == '"' then offset = scanStringEnd(source, offset + 1)
      else if source.charAt(offset) == '\'' then offset = scanCharEnd(source, offset + 1)
      else offset += 1

    comments.toList

  private def scanBlockCommentEnd(source: String, from: Int): Int =
    var offset = from
    var depth  = 1

    while offset < source.length && depth > 0 do
      if startsWith(source, offset, "{-") then
        depth += 1
        offset += 2
      else if startsWith(source, offset, "-}") then
        depth -= 1
        offset += 2
      else offset += 1

    offset

  private def scanStringEnd(source: String, from: Int): Int =
    var offset  = from
    var escaped = false

    while offset < source.length do
      val c = source.charAt(offset)
      if escaped then escaped = false
      else if c == '\\' then escaped = true
      else if c == '"' then return offset + 1
      offset += 1

    offset

  private def scanCharEnd(source: String, from: Int): Int =
    var offset  = from
    var escaped = false

    while offset < source.length do
      val c = source.charAt(offset)
      if escaped then escaped = false
      else if c == '\\' then escaped = true
      else if c == '\'' then return offset + 1
      offset += 1

    offset

  private def startsWith(source: String, offset: Int, prefix: String): Boolean =
    source.regionMatches(offset, prefix, 0, prefix.length)
