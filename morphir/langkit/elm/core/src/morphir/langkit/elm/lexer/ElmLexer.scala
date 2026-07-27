package morphir.langkit.elm.lexer

import parsley.Parsley
import parsley.Parsley.{atomic, eof, many, some}
import parsley.character.{char, digit, letter, noneOf, satisfy, string, stringOfSome}
import parsley.combinator.option
import parsley.errors.combinator.ErrorMethods
import parsley.token.Lexer
import parsley.token.descriptions.{LexicalDesc, NameDesc, SpaceDesc, SymbolDesc}
import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
import parsley.token.predicate

/**
 * Provides tokenisation primitives for the Elm language dialect.
 *
 * Elm is indentation-sensitive, so newlines are structurally significant. The lexer handles identifiers (lower and
 * upper), operators, keywords, numeric and string literals, and whitespace management.
 */
object ElmLexer:

  // -----------------------------------------------------------------------
  // Token Lexer configuration
  // -----------------------------------------------------------------------

  /**
   * Elm reserved keywords. Exposed so tooling (editor tokenizers, doc generators) can stay in sync with the parser's
   * notion of "keyword" without duplicating the list.
   */
  val keywords: Set[String] = Set(
    "module",
    "exposing",
    "import",
    "as",
    "port",
    "effect",
    "type",
    "alias",
    "let",
    "in",
    "if",
    "then",
    "else",
    "case",
    "of",
    "infix",
    "left",
    "right",
    "non",
    "where"
  )

  /**
   * Elm hard operators. Exposed so tooling (editor tokenizers, doc generators) can stay in sync with the parser's
   * notion of "operator" without duplicating the list.
   */
  val operators: Set[String] = Set(
    "->",
    "<-",
    "::",
    "=",
    "|",
    "\\",
    ".",
    "..",
    "+",
    "-",
    "*",
    "/",
    "//",
    "^",
    "==",
    "/=",
    "<",
    ">",
    "<=",
    ">=",
    "&&",
    "||",
    "++",
    "<|",
    "|>",
    ">>",
    "<<"
  )

  /**
   * Characters an Elm operator may be built from, matching `binopCharSet` in `elm/compiler`'s `Parse.Symbol`.
   *
   * Note what is absent: `~` is not an Elm operator character at all, and `\` belongs to lambda syntax rather than to
   * operators, so neither may appear inside one.
   */
  val operatorCharacters: Set[Char] = "+-/*=.<>:&|^?%!".toSet

  /**
   * Symbol sequences Elm reserves, so they can never be used as binary operators — `BadDot`, `BadPipe`, `BadArrow`,
   * `BadEquals` and `BadHasType` in `Parse.Symbol`. They remain valid *structural* tokens: `=` separates a declaration
   * from its body, `->` a case branch from its result, and so on.
   */
  val reservedOperators: Set[String] = Set(".", "|", "->", "=", ":")

  private lazy val hardOperator: Parsley[String] =
    operators.toList.sortBy(op => -op.length).map(op => symbol(op).as(op)).reduce(_ | _)

  private val desc: LexicalDesc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(c => c.isLetter || c == '_'),
      identifierLetter = predicate.Basic(c => c.isLetterOrDigit || c == '_'),
      operatorStart = predicate.Basic(operatorCharacters.contains),
      operatorLetter = predicate.Basic(operatorCharacters.contains)
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = keywords,
      hardOperators = operators
    ),
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = "--",
      commentStart = "{-",
      commentEnd = "-}",
      nestedComments = true
    )
  )

  private val lexer: Lexer = new Lexer(desc)

  // -----------------------------------------------------------------------
  // Identifiers
  // -----------------------------------------------------------------------

  /** A lower-case identifier: starts with a lowercase letter or underscore. */
  val lowerIdentifier: Parsley[String] =
    atomic(lexer.lexeme.names.identifier.filter(s => s.head.isLower || s.head == '_'))

  /** An upper-case identifier: starts with an uppercase letter. */
  val upperIdentifier: Parsley[String] =
    atomic(lexer.lexeme.names.identifier.filter(_.head.isUpper))

  /** Any identifier (lower or upper). */
  val identifier: Parsley[String] = lexer.lexeme.names.identifier

  /** A user-defined operator. */
  val operator: Parsley[String] = hardOperator | lexer.lexeme.names.userDefinedOperator

  // -----------------------------------------------------------------------
  // Keywords and symbols
  // -----------------------------------------------------------------------

  /** Parse a specific keyword. */
  def keyword(kw: String): Parsley[Unit] = lexer.lexeme.symbol(kw)

  /** Parse a specific symbol/operator. */
  def symbol(sym: String): Parsley[Unit] = lexer.lexeme.symbol(sym)

  // -----------------------------------------------------------------------
  // Literals
  // -----------------------------------------------------------------------

  /** An integer literal. */
  val intLiteral: Parsley[Long] = lexer.lexeme.integer.decimal64

  /** A floating-point literal. */
  val floatLiteral: Parsley[Double] = lexer.lexeme.floating.decimalDouble

  /** A string literal. */
  val stringLiteral: Parsley[String] = lexer.lexeme.string.ascii

  /** A character literal. */
  val charLiteral: Parsley[Char] = lexer.lexeme.character.ascii

  // -----------------------------------------------------------------------
  // Whitespace and structure
  // -----------------------------------------------------------------------

  /** Fully wraps a parser: skips leading whitespace and asserts end-of-input. */
  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

  /** Matches a single space or tab (horizontal whitespace). */
  val hspace: Parsley[Char] = satisfy(c => c == ' ' || c == '\t')

  /** Skips zero or more horizontal whitespace characters. */
  val hspaces: Parsley[Unit] = many(hspace).void

  /** Matches end-of-line or end-of-input. */
  val eolOrEof: Parsley[Unit] = (char('\n').void | (string("\r\n").void) | eof).label("end of line")

  /** Matches a newline character. */
  val newline: Parsley[Char] = char('\n')

  /** Parse content wrapped in parentheses. */
  def parens[A](p: Parsley[A]): Parsley[A] = lexer.lexeme.parens(p)

  /** Parse content wrapped in square brackets. */
  def brackets[A](p: Parsley[A]): Parsley[A] = lexer.lexeme.brackets(p)

  /** Parse content wrapped in curly braces. */
  def braces[A](p: Parsley[A]): Parsley[A] = lexer.lexeme.braces(p)

  /** Parse a comma-separated list. */
  def commaSep[A](p: Parsley[A]): Parsley[List[A]] =
    lexer.lexeme.commaSep(p)

  /** Parse a comma-separated list with at least one element. */
  def commaSep1[A](p: Parsley[A]): Parsley[List[A]] =
    lexer.lexeme.commaSep1(p)
