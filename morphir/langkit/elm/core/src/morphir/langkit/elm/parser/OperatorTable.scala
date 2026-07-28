package morphir.langkit.elm.parser

import morphir.langkit.elm.cst.{Associativity, CstInfixDeclaration, CstModule}

/** How tightly a binary operator binds, and which way a chain of it groups. */
final case class Fixity(precedence: Int, associativity: Associativity) derives CanEqual

/**
 * The fixity of every binary operator in scope, used by [[OperatorReassociator]] to shape operator chains.
 *
 * Elm has no fixed operator set: `infix` declarations introduce operators and give them a precedence (0-9) and an
 * associativity. Only packages may declare them, so this table is built from a bundled set — [[builtin]] for
 * `elm/core`, [[wellKnown]] adding the other official packages that declare operators — overlaid with whatever the
 * module being parsed declares itself.
 *
 * An operator declared in a package that is not bundled is not visible here: resolving it needs that dependency's
 * source, which the parser does not have. `lookup` returns `None` for it, and the caller decides — by default a
 * diagnostic, since guessing a fixity silently mis-groups the expression around it.
 */
final case class OperatorTable(fixities: Map[String, Fixity]) derives CanEqual:

  /** The fixity of `operator`, if anything in scope declares it. */
  def lookup(operator: String): Option[Fixity] = fixities.get(operator)

  /** The fixity of `operator`, falling back to [[OperatorTable.unknownFixity]] when nothing declares it. */
  def fixityOf(operator: String): Fixity =
    fixities.getOrElse(operator, OperatorTable.unknownFixity)

  /** Overlay `infix` declarations onto this table; a declaration wins over a built-in of the same name. */
  def withInfixDeclarations(declarations: Iterable[CstInfixDeclaration]): OperatorTable =
    OperatorTable(
      fixities ++ declarations.map(d => d.operator.value -> Fixity(d.precedence, d.associativity))
    )

object OperatorTable:

  /**
   * Fixity assumed for an operator no `infix` declaration in scope names, when the caller has asked for that rather
   * than a diagnostic.
   *
   * Binding tightest-and-left keeps such an operator from silently swallowing its neighbours' operands, but the
   * grouping is still a guess: see `ElmParseOptions.unknownOperator`.
   */
  val unknownFixity: Fixity = Fixity(9, Associativity.Left)

  /**
   * The `infix` declarations of `elm/core`: `Basics` plus `List`'s `(::)`.
   *
   * Transcribed from `Basics.elm`, which is worth quoting because the composition pair reads backwards to most people —
   * `<<` composes right-to-left and is *left*-associative, `>>` composes left-to-right and is *right*- associative:
   *
   * {{{
   * infix right 0 (<|) = apL      infix left  6 (+)  = add
   * infix left  0 (|>) = apR      infix left  6 (-)  = sub
   * infix right 2 (||) = or       infix left  7 (*)  = mul
   * infix right 3 (&&) = and      infix left  7 (/)  = fdiv
   * infix non   4 (==) = eq       infix left  7 (//) = idiv
   * infix non   4 (/=) = neq      infix right 8 (^)  = pow
   * infix non   4 (<)  = lt       infix left  9 (<<) = composeL
   * infix non   4 (>)  = gt       infix right 9 (>>) = composeR
   * infix non   4 (<=) = le
   * infix non   4 (>=) = ge       -- List.elm
   * infix right 5 (++) = append   infix right 5 (::) = cons
   * }}}
   */
  val builtin: OperatorTable = OperatorTable(
    Map(
      "<|" -> Fixity(0, Associativity.Right),
      "|>" -> Fixity(0, Associativity.Left),
      "||" -> Fixity(2, Associativity.Right),
      "&&" -> Fixity(3, Associativity.Right),
      "==" -> Fixity(4, Associativity.Non),
      "/=" -> Fixity(4, Associativity.Non),
      "<"  -> Fixity(4, Associativity.Non),
      ">"  -> Fixity(4, Associativity.Non),
      "<=" -> Fixity(4, Associativity.Non),
      ">=" -> Fixity(4, Associativity.Non),
      "++" -> Fixity(5, Associativity.Right),
      "::" -> Fixity(5, Associativity.Right),
      "+"  -> Fixity(6, Associativity.Left),
      "-"  -> Fixity(6, Associativity.Left),
      "*"  -> Fixity(7, Associativity.Left),
      "/"  -> Fixity(7, Associativity.Left),
      "//" -> Fixity(7, Associativity.Left),
      "^"  -> Fixity(8, Associativity.Right),
      "<<" -> Fixity(9, Associativity.Left),
      ">>" -> Fixity(9, Associativity.Right)
    )
  )

  /** The `infix` declarations of `elm/parser`'s `Parser` (and `Parser.Advanced`, which repeats them). */
  val elmParser: OperatorTable = OperatorTable(
    Map(
      "|=" -> Fixity(5, Associativity.Left),
      "|." -> Fixity(6, Associativity.Left)
    )
  )

  /** The `infix` declarations of `elm/url`'s `Url.Parser`. */
  val elmUrl: OperatorTable = OperatorTable(
    Map(
      "</>" -> Fixity(7, Associativity.Right),
      "<?>" -> Fixity(8, Associativity.Left)
    )
  )

  /**
   * Every official Elm package that declares operators: [[builtin]], [[elmParser]] and [[elmUrl]].
   *
   * Fixities are matched by operator name, not by resolving imports — the parser has no dependency source to resolve
   * against. A module that declares `(|=)` itself still wins, since its own declarations are overlaid last.
   */
  val wellKnown: OperatorTable = OperatorTable(builtin.fixities ++ elmParser.fixities ++ elmUrl.fixities)

  /** The table in scope for `module`: `base` overlaid with the module's own `infix` declarations. */
  def forModule(module: CstModule, base: OperatorTable = wellKnown): OperatorTable =
    base.withInfixDeclarations(module.declarations.collect { case d: CstInfixDeclaration => d })
