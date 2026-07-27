package morphir.langkit.elm.lexer

import parsley.Parsley
import parsley.Parsley.{atomic, eof, lookAhead, many, some}
import parsley.character.{char, digit, letter, noneOf, satisfy, string, stringOfSome}
import parsley.combinator.option
import parsley.errors.combinator.ErrorMethods
import parsley.position.pos
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

  /**
   * Tokens that do not consume the whitespace after them.
   *
   * Elm's grammar asks whether two tokens are *adjacent* in several places — `a.b` is field access while `a . b` is an
   * error, `List.map` is a qualified name while `List . map` is not — and a token that swallows its own trailing
   * whitespace has destroyed that information by the time the next production runs. Productions that care compose these
   * and consume [[whiteSpace]] themselves, once, at the point where whitespace is actually permitted.
   */
  object raw:
    val lowerIdentifier: Parsley[String] =
      atomic(lexer.nonlexeme.names.identifier.filter(s => s.head.isLower || s.head == '_'))

    val upperIdentifier: Parsley[String] =
      atomic(lexer.nonlexeme.names.identifier.filter(_.head.isUpper))

    val identifier: Parsley[String] = lexer.nonlexeme.names.identifier

    val intLiteral: Parsley[Long]      = lexer.nonlexeme.integer.decimal64
    val floatLiteral: Parsley[Double]  = lexer.nonlexeme.floating.decimalDouble
    val stringLiteral: Parsley[String] = lexer.nonlexeme.string.ascii
    val charLiteral: Parsley[Char]     = lexer.nonlexeme.character.ascii

    /** A literal character, for the brackets and punctuation whose adjacency matters. */
    def sym(c: Char): Parsley[Unit] = char(c).void

  /** Skip whitespace and comments. Productions built from [[raw]] tokens call this where whitespace is permitted. */
  val whiteSpace: Parsley[Unit] = lexer.space.whiteSpace

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

  // -----------------------------------------------------------------------
  // Layout
  // -----------------------------------------------------------------------

  /**
   * Elm is indentation-sensitive: a top-level declaration begins in column 1, and the items of a `let` or `case` block
   * line up with each other. These combinators are how a production states the column it requires, which is also how it
   * knows where its block ends — the first item that breaks the alignment belongs to whatever encloses it.
   */

  /** Succeed without consuming input when the next token begins in `column`. */
  def atColumn(column: Int): Parsley[Unit] =
    lookAhead(pos.filter { case (_, col) => col == column }).void

  /** Succeed without consuming input when the next token begins in column 1, where a declaration must. */
  val atTopLevel: Parsley[Unit] = atColumn(1)

  /**
   * One or more `p`, every one after the first beginning in the same column as the first.
   *
   * This is the shape of a `let` block and of the branches of a `case`: Elm reads them as a block for exactly as long
   * as they line up, and the first token that does not is something else's.
   */
  def aligned[A](p: Parsley[A]): Parsley[List[A]] = (pos <~> p).flatMap { case ((_, column), first) =>
    many(atomic(atColumn(column) *> p)).map(first :: _)
  }

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
