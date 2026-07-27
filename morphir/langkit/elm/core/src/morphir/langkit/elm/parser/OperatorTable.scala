package morphir.langkit.elm.parser

import morphir.langkit.elm.cst.{Associativity, CstInfixDeclaration, CstModule}

/** How tightly a binary operator binds, and which way a chain of it groups. */
final case class Fixity(precedence: Int, associativity: Associativity) derives CanEqual

/**
 * The fixity of every binary operator in scope, used by [[OperatorReassociator]] to shape operator chains.
 *
 * Elm has no fixed operator set: `infix` declarations introduce operators and give them a precedence (0-9) and an
 * associativity. Only `elm/core` may declare them in practice, but the syntax is general, so this table is built from
 * [[builtin]] — the `infix` declarations of `Basics` and `List` — overlaid with whatever the module being parsed
 * declares itself.
 *
 * Operators declared in *another* module and imported are not visible here: resolving them needs the dependency's
 * source, which the parser does not have. Such operators fall back to [[OperatorTable.unknownFixity]], which groups
 * them tighter than everything and to the left.
 */
final case class OperatorTable(fixities: Map[String, Fixity]) derives CanEqual:

  /** The fixity of `operator`, or [[OperatorTable.unknownFixity]] if nothing in scope declares it. */
  def fixityOf(operator: String): Fixity =
    fixities.getOrElse(operator, OperatorTable.unknownFixity)

  /** Overlay `infix` declarations onto this table; a declaration wins over a built-in of the same name. */
  def withInfixDeclarations(declarations: Iterable[CstInfixDeclaration]): OperatorTable =
    OperatorTable(
      fixities ++ declarations.map(d => d.operator.value -> Fixity(d.precedence, d.associativity))
    )

object OperatorTable:

  /**
   * Fixity assumed for an operator no `infix` declaration in scope names.
   *
   * Elm would reject the expression outright; the parser cannot, because the declaration may live in a module it never
   * sees. Binding tightest-and-left keeps such an operator from silently swallowing its neighbours' operands.
   */
  val unknownFixity: Fixity = Fixity(9, Associativity.Left)

  /** The `infix` declarations of `elm/core`: `Basics` plus `List`'s `(::)`. */
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

  /** The table in scope for `module`: the built-ins overlaid with the module's own `infix` declarations. */
  def forModule(module: CstModule): OperatorTable =
    builtin.withInfixDeclarations(module.declarations.collect { case d: CstInfixDeclaration => d })
