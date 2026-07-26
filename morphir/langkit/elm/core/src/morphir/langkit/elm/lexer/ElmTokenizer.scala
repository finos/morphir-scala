package morphir.langkit.elm.lexer

import morphir.langkit.elm.compiler.CompileError
import morphir.langkit.elm.compiler.ParseDiagnostic
import morphir.langkit.trees.query.QueryLogic

final case class ElmTokenizerConfig(
    includeTrivia: Boolean,
    recoverUnknown: Boolean
) derives CanEqual

final case class ElmTokenizerCtx(
    config: ElmTokenizerConfig
) derives CanEqual

enum ElmTokenKind derives CanEqual:

  case Keyword, LowerIdentifier, UpperIdentifier, Operator, Number, StringLiteral, CharLiteral, Comment, Whitespace,
    Newline, Punctuation, Unknown

final case class ElmToken(kind: ElmTokenKind, lexeme: String, start: Int, end: Int) derives CanEqual

object ElmTokenizer:
  type TokenizeCtx       = ElmTokenizerCtx
  type TokenizeLog       = String
  type TokenizeErr       = CompileError
  type TokenizeEff[A]    = QueryLogic.QueryEffect[TokenizeCtx, TokenizeLog, TokenizeErr, A]
  type TokenizeResult[A] = QueryLogic.Result[TokenizeCtx, TokenizeLog, TokenizeErr, A]

  val defaultConfig: ElmTokenizerConfig = ElmTokenizerConfig(
    includeTrivia = false,
    recoverUnknown = true
  )

  val defaultContext: ElmTokenizerCtx = ElmTokenizerCtx(defaultConfig)

  private val hardOperators: Vector[String] = ElmLexer.operators.toVector.sortBy(op => (-op.length, op))
  private val punctuation: Set[Char]        = Set('(', ')', '[', ']', '{', '}', ',', ';')
  private val operatorChars: Set[Char]      = "+-*/<>=&|^!~%?:.\\".toSet

  def tokenize(source: String): TokenizeEff[Vector[ElmToken]] =
    for
      ctx <- QueryLogic.readContext[TokenizeCtx, TokenizeLog, TokenizeErr]
      out <- scan(source, ctx.config)
    yield out

  def run(source: String): TokenizeResult[Vector[ElmToken]] =
    run(source, defaultContext)

  def run(source: String, config: ElmTokenizerConfig): TokenizeResult[Vector[ElmToken]] =
    run(source, ElmTokenizerCtx(config))

  def run(source: String, ctx: ElmTokenizerCtx): TokenizeResult[Vector[ElmToken]] =
    QueryLogic.run[TokenizeCtx, TokenizeLog, TokenizeErr, Vector[ElmToken]](ctx)(tokenize(source))

  private def scan(source: String, config: ElmTokenizerConfig): TokenizeEff[Vector[ElmToken]] =
    scanLoop(source, config, index = 0, acc = Vector.empty)

  private def scanLoop(
      source: String,
      config: ElmTokenizerConfig,
      index: Int,
      acc: Vector[ElmToken]
  ): TokenizeEff[Vector[ElmToken]] =
    if index >= source.length then acc
    else
      val start = index
      val ch    = source.charAt(index)

      if source.startsWith("\r\n", index) then
        scanLoop(
          source,
          config,
          index + 2,
          appendToken(source, config, acc, ElmTokenKind.Newline, start, index + 2)
        )
      else if ch == '\n' || ch == '\r' then
        scanLoop(
          source,
          config,
          index + 1,
          appendToken(source, config, acc, ElmTokenKind.Newline, start, index + 1)
        )
      else if ch == ' ' || ch == '\t' then
        val end = consumeWhile(source, index)(c => c == ' ' || c == '\t')
        scanLoop(source, config, end, appendToken(source, config, acc, ElmTokenKind.Whitespace, start, end))
      else if source.startsWith("--", index) then
        val end = consumeLineComment(source, index)
        scanLoop(source, config, end, appendToken(source, config, acc, ElmTokenKind.Comment, start, end))
      else if source.startsWith("{-", index) then
        val end = consumeBlockComment(source, index)
        scanLoop(source, config, end, appendToken(source, config, acc, ElmTokenKind.Comment, start, end))
      else if ch == '"' then
        val end = consumeQuoted(source, index, '"')
        scanLoop(source, config, end, appendToken(source, config, acc, ElmTokenKind.StringLiteral, start, end))
      else if ch == '\'' then
        val end = consumeQuoted(source, index, '\'')
        scanLoop(source, config, end, appendToken(source, config, acc, ElmTokenKind.CharLiteral, start, end))
      else if ch.isDigit then
        val end = consumeNumber(source, index)
        scanLoop(source, config, end, appendToken(source, config, acc, ElmTokenKind.Number, start, end))
      else if isIdentifierStart(ch) then
        val end    = consumeWhile(source, index)(isIdentifierPart)
        val lexeme = source.substring(start, end)
        val kind   =
          if ElmLexer.keywords.contains(lexeme) then ElmTokenKind.Keyword
          else if lexeme.head.isUpper then ElmTokenKind.UpperIdentifier
          else ElmTokenKind.LowerIdentifier
        scanLoop(source, config, end, appendToken(source, config, acc, kind, start, end))
      else
        hardOperators.find(source.startsWith(_, index)) match
          case Some(op) =>
            val end = index + op.length
            scanLoop(
              source,
              config,
              end,
              appendToken(source, config, acc, ElmTokenKind.Operator, start, end)
            )
          case None if operatorChars.contains(ch) =>
            val end = consumeWhile(source, index)(operatorChars.contains)
            scanLoop(
              source,
              config,
              end,
              appendToken(source, config, acc, ElmTokenKind.Operator, start, end)
            )
          case None if punctuation.contains(ch) =>
            scanLoop(
              source,
              config,
              index + 1,
              appendToken(source, config, acc, ElmTokenKind.Punctuation, start, index + 1)
            )
          case None =>
            for
              recovered <- recoverUnknown(source, start, config)
              out       <- scanLoop(source, config, start + 1, acc ++ recovered)
            yield out

  private def appendToken(
      source: String,
      config: ElmTokenizerConfig,
      acc: Vector[ElmToken],
      kind: ElmTokenKind,
      start: Int,
      end: Int
  ): Vector[ElmToken] =
    if config.includeTrivia || !isTrivia(kind) then acc :+ ElmToken(kind, source.substring(start, end), start, end)
    else acc

  private def recoverUnknown(source: String, start: Int, config: ElmTokenizerConfig): TokenizeEff[Vector[ElmToken]] =
    val lexeme = source.substring(start, start + 1)
    val err    = CompileError.ParseError(
      phase = "tokenize",
      diagnostic = ParseDiagnostic.tokenizerUnexpectedCharacter(source, start, lexeme)
    )
    if config.recoverUnknown then
      for _ <- QueryLogic.log[TokenizeCtx, TokenizeLog, TokenizeErr](
          s"Recovered unknown token '$lexeme' at $start"
        )
      yield Vector(ElmToken(ElmTokenKind.Unknown, lexeme, start, start + 1))
    else QueryLogic.failFast[TokenizeCtx, TokenizeLog, TokenizeErr](err)

  private def isTrivia(kind: ElmTokenKind): Boolean =
    kind == ElmTokenKind.Whitespace || kind == ElmTokenKind.Newline || kind == ElmTokenKind.Comment

  private def isIdentifierStart(ch: Char): Boolean =
    ch.isLetter || ch == '_'

  private def isIdentifierPart(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == '_'

  private def consumeWhile(source: String, index: Int)(p: Char => Boolean): Int =
    var cursor = index
    while cursor < source.length && p(source.charAt(cursor)) do cursor += 1
    cursor

  private def consumeLineComment(source: String, index: Int): Int =
    var cursor = index + 2
    while cursor < source.length && source.charAt(cursor) != '\n' && source.charAt(cursor) != '\r' do cursor += 1
    cursor

  private def consumeBlockComment(source: String, index: Int): Int =
    var cursor = index + 2
    var depth  = 1
    while cursor < source.length && depth > 0 do
      if source.startsWith("{-", cursor) then
        depth += 1
        cursor += 2
      else if source.startsWith("-}", cursor) then
        depth -= 1
        cursor += 2
      else cursor += 1
    cursor

  private def consumeQuoted(source: String, index: Int, quote: Char): Int =
    var cursor  = index + 1
    var escaped = false
    while cursor < source.length do
      val ch = source.charAt(cursor)
      cursor += 1
      if escaped then escaped = false
      else if ch == '\\' then escaped = true
      else if ch == quote then return cursor
    cursor

  private def consumeNumber(source: String, index: Int): Int =
    var cursor = consumeWhile(source, index)(_.isDigit)
    if cursor + 1 < source.length && source.charAt(cursor) == '.' && source.charAt(cursor + 1).isDigit then
      cursor = consumeWhile(source, cursor + 1)(_.isDigit)
    cursor
