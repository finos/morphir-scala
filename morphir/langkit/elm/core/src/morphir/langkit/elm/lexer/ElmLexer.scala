package morphir.langkit.elm.lexer

import parsley.Parsley
import parsley.Parsley.{atomic, eof, lookAhead, many, notFollowedBy, some}
import parsley.character.{char, digit, letter, noneOf, satisfy, string, stringOfSome}
import parsley.combinator.option
import parsley.errors.combinator.ErrorMethods
import parsley.position.pos
import parsley.token.Lexer
import parsley.token.descriptions.{LexicalDesc, NameDesc, SpaceDesc, SymbolDesc}
import parsley.token.descriptions.numeric.{BreakCharDesc, NumericDesc, PlusSignPresence}
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
    numericDesc = NumericDesc.plain.copy(
      literalBreakChar = BreakCharDesc.NoBreakChar,
      leadingDotAllowed = false,
      trailingDotAllowed = false,
      leadingZerosAllowed = false,
      positiveSign = PlusSignPresence.Illegal,
      integerNumbersCanBeHexadecimal = true,
      integerNumbersCanBeOctal = false,
      integerNumbersCanBeBinary = false,
      realNumbersCanBeHexadecimal = false,
      realNumbersCanBeOctal = false,
      realNumbersCanBeBinary = false,
      hexadecimalLeads = Set('x'),
      octalLeads = Set.empty,
      binaryLeads = Set.empty
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
  // Text literals
  // -----------------------------------------------------------------------

  /**
   * Elm's string and character literals, written out rather than configured.
   *
   * `parsley.token`'s escape machinery can describe a numeric escape with a single-character prefix — `\uXXXX` — but
   * not one wrapped in braces, and Elm's is `\u{XXXX}`. Rather than accept a near-miss, the literals below follow
   * `elm/compiler`'s `Parse.String` directly: six single-character escapes, the braced unicode escape, and
   * triple-quoted strings that take newlines and lone quotes as they come.
   */
  private object text:

    /** The six single-character escapes Elm knows, and nothing else. */
    private val simpleEscape: Parsley[Int] =
      char('n').as(0x0a)
        | char('r').as(0x0d)
        | char('t').as(0x09)
        | char('"').as(0x22)
        | char('\'').as(0x27)
        | char('\\').as(0x5c)

    private val hexDigit: Parsley[Char] =
      satisfy(c => c.isDigit || ('a' to 'f').contains(c) || ('A' to 'F').contains(c))

    /**
     * `\u{XXXX}` — between four and six hexadecimal digits, naming a code point.
     *
     * Elm rejects both a shorter run and a longer one, and anything above the last valid code point.
     */
    private val unicodeEscape: Parsley[Int] = (string("u{") *> stringOfSome(hexDigit) <* char('}')).collectMsg(digits =>
      Seq(s"\\u{$digits} is not a valid unicode escape: it needs 4 to 6 hexadecimal digits naming a code point")
    ) {
      case digits
          if digits.length >= 4 && digits.length <= 6 &&
            Character.isValidCodePoint(Integer.parseInt(digits, 16)) =>
        Integer.parseInt(digits, 16)
    }

    private val escape: Parsley[Int] = char('\\') *> (simpleEscape | unicodeEscape)

    /** A character of a single-quoted string: anything but the delimiter, a backslash, or a line break. */
    private val stringCharacter: Parsley[Int] =
      escape | satisfy(c => c != '"' && c != '\\' && c != '\n' && c != '\r').map(_.toInt)

    /**
     * A character of a triple-quoted string, where a lone quote and a line break are ordinary content.
     *
     * The quote case has to come with its own guard and exclusion: a `"` is content only when it is not the start of
     * the closing delimiter, and the catch-all below must not take one either, or `many` would swallow the delimiter
     * and run to the end of the file.
     */
    private val multiStringCharacter: Parsley[Int] =
      escape
        | atomic(char('"') <* notFollowedBy(string("\"\""))).map(_.toInt)
        | satisfy(c => c != '\\' && c != '"').map(_.toInt)

    private def codePoints(chars: List[Int]): String =
      chars.foldLeft(new java.lang.StringBuilder)(_.appendCodePoint(_)).toString

    private val multiString: Parsley[String] =
      atomic(string("\"\"\"")) *> many(multiStringCharacter).map(codePoints) <* string("\"\"\"")

    private val singleString: Parsley[String] =
      char('"') *> many(stringCharacter).map(codePoints) <* char('"')

    val literal: Parsley[String] = (multiString | singleString).label("string literal")

    /**
     * A character literal.
     *
     * Elm allows any code point here, including one outside the basic multilingual plane. The CST models a character as
     * a JVM `Char`, so an astral code point cannot be represented — that gap is recorded in the conformance plan rather
     * than silently truncated, and rejected here.
     */
    val character: Parsley[Char] =
      (char('\'') *> (escape | satisfy(c => c != '\'' && c != '\\').map(_.toInt)) <* char('\'')).collectMsg(point =>
        Seq(
          s"the character U+${point.toHexString.toUpperCase} is outside the range this parser can represent " +
            "(see the astral-code-point gap in the conformance plan)"
        )
      ) { case point if point <= Char.MaxValue.toInt => point.toChar }
        .label("character literal")

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

    val intLiteral: Parsley[Long]      = lexer.nonlexeme.integer.number64
    val floatLiteral: Parsley[Double]  = lexer.nonlexeme.floating.decimalDouble
    val stringLiteral: Parsley[String] = text.literal
    val charLiteral: Parsley[Char]     = text.character

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

  /** An integer literal, decimal or hexadecimal (`0x1f`). */
  val intLiteral: Parsley[Long] = lexer.lexeme.integer.number64

  /** A floating-point literal, with an optional exponent. */
  val floatLiteral: Parsley[Double] = lexer.lexeme.floating.decimalDouble

  /** A string literal, single- or triple-quoted. */
  val stringLiteral: Parsley[String] = text.literal <* whiteSpace

  /** A character literal. */
  val charLiteral: Parsley[Char] = text.character <* whiteSpace

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
