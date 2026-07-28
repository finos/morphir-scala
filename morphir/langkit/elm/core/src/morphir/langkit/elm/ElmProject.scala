package morphir.langkit.elm

import kyo.Frame

import morphir.langkit.elm.cst.{CstInfixDeclaration, CstModule}
import morphir.langkit.elm.parser.{Fixity, OperatorTable}

/**
 * Parsing several Elm modules that know about each other.
 *
 * A single module cannot say what `(|=)` means: the fixity is declared somewhere else, and `ElmParse` reports
 * `ELM-P005` rather than guessing. That is right for one module read on its own, and wrong for a project, where the
 * declaration is usually sitting in a sibling module the caller already has.
 *
 * So this reads the whole set twice. The first pass takes each module's `infix` declarations — that needs only the
 * syntax stage, since a fixity declaration says what it means without reference to anything. The second pass parses
 * each module for real, against a table assembled from the modules it imports.
 *
 * What remains beyond reach is an operator from a package whose source the caller does not have. For that,
 * `OperatorTable.wellKnown` covers the official packages and `ElmParseOptions.operators` takes anything else.
 */
object ElmProject:

  /** What parsing a project produced, keyed by module name. */
  final case class Outcome(
      modules: Map[String, ElmParse.Outcome[CstModule]],
      unparsed: Map[String, ElmParse.Outcome[CstModule]]
  ) derives CanEqual:
    /** Every module that produced a tree. */
    def trees: Map[String, CstModule] = modules.collect {
      case (name, outcome) if outcome.isSuccess =>
        name -> outcome.value.get
    }

    def isSuccess: Boolean = unparsed.isEmpty && modules.forall(_._2.isSuccess)

  /**
   * Parse `sources` as one project.
   *
   * Sources are keyed by whatever the caller calls them — a path, usually — because a module that does not parse has no
   * name to be keyed by. Modules that fail the first pass appear in `unparsed`, and take no part in resolution.
   */
  def parse(
      sources: Map[String, String],
      options: ElmParseOptions = ElmParseOptions.elm
  )(using Frame): Outcome =
    val surveyed = sources.view.mapValues(survey(_, options)).toMap

    val (readable, unreadable) = surveyed.partition { case (_, outcome) => outcome.isSuccess }

    // What each module declares, by module name, so an importer can look it up.
    val declaredByModule: Map[String, Map[String, Fixity]] =
      readable.values.flatMap(_.value).map { module =>
        moduleName(module) -> module.declarations.collect { case d: CstInfixDeclaration =>
          d.operator.value -> Fixity(d.precedence, d.associativity)
        }.toMap
      }.toMap

    val parsed = readable.collect { case (key, outcome) =>
      val module      = outcome.value.get
      val fromImports = module.imports.flatMap { imported =>
        declaredByModule.getOrElse(imported.moduleName.parts.map(_.value).mkString("."), Map.empty)
      }.toMap

      val table = OperatorTable(options.operators.fixities ++ fromImports)
      moduleName(module) -> ElmParse.run(options.withOperators(table))(ElmParse.cst(sources(key)))
    }

    Outcome(modules = parsed, unparsed = unreadable)

  /**
   * A first look at a module: enough to learn its name, its imports, and what it declares `infix`.
   *
   * Run leniently on purpose. An operator this module *uses* may be unresolvable until the second pass — that is the
   * whole point — so refusing to read it now would hide the very declarations that fix it.
   */
  private def survey(source: String, options: ElmParseOptions)(using Frame): ElmParse.Outcome[CstModule] =
    ElmParse.run(options.withUnknownOperator(Leniency.Accept).withOperatorChainConflict(Leniency.Accept))(
      ElmParse.cst(source)
    )

  private def moduleName(module: CstModule): String =
    module.moduleDecl.name.parts.map(_.value).mkString(".")
