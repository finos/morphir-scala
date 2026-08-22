package morphir.langkit.markdown

/**
 * One door into the Markdown langkit: `import morphir.langkit.markdown.MD`, then `import MD.*`, and the module is in
 * scope — the AST, the authoring combinators, the vocabulary the verbs read, and the two verb namespaces that parse and
 * write.
 *
 * {{{
 * import morphir.langkit.markdown.MD
 * import MD.*
 * import MD.given // string literals as Text; a Scala 3 wildcard import does not carry givens
 *
 * val page = doc(h1("Title"), p("hello ", strong("world")))
 * val text = writer.write(page)
 * val back = parser.parse(text)
 * }}}
 *
 * '''The verbs live in namespaces, not at the top.''' [[MD.parser]] parses, [[MD.writer]] writes and [[MD.cst]] reads
 * and lowers the concrete syntax tree; there is no flat `MD.parse`. Every object behind them —
 * `morphir.langkit.markdown.internal.Parser`, `internal.MdWriter`, `internal.CstParser`, `internal.Cst` and
 * `internal.Lower` — is `private[markdown]` machinery, and this is the only way to them from outside the module.
 * Grouping them also keeps `parse` and `write` from landing as bare names in a file that did `import MD.*`, where they
 * would say nothing about what they parse or write.
 *
 * '''Which half authors, which half you match on.''' `MD.*` deliberately holds two spellings of the same tree in one
 * namespace. The lowercase combinators — `doc`, `h1`, `p`, `ul`, `li`, `a`, `em`, `strong` — are the authoring door;
 * they take varargs, accept string literals as text, and fill in [[MdMeta.empty]] for you. The PascalCase names —
 * `Root`, `Paragraph`, `Heading`, `Text` — are [[MdNode]]'s own cases: what a parse hands back and what you match on.
 * Write `p("hi")`, match `case Paragraph(children, _)`. Nothing stops you constructing a `Paragraph` directly, and for
 * a node whose meta you are setting deliberately that is the right call; for authoring, the combinator is shorter and
 * says so.
 *
 * '''`List` means [[MdNode.List]] here.''' A wildcard import shadows `scala.collection.immutable.List`, which the DSL
 * already chose on purpose so `ul`/`ol` can return the node's own name; `MD.*` inherits that choice rather than
 * introducing it. Reach for `scala.List` under a different name if you need it in the same file — or, since Markdown
 * trees are [[kyo.Chunk]]s throughout, find that you do not.
 *
 * '''Types are flat, verbs are nested.''' `MD.*` carries the AST cases, the two tree types [[MdNode]] and
 * [[MdCstNode]], the combinators and the config vocabulary. The verbs sit in [[MD.parser]], [[MD.writer]] and
 * [[MD.cst]], named by what they read or produce. The one place that rule bends is the CST's *cases*: `MdCstNode` names
 * a `Paragraph`, a `Heading` and a `Text` of its own, so its cases stay behind their companion and are reached as
 * `MdCstNode.Document` rather than flattened into a collision.
 *
 * Nothing under `morphir.langkit.markdown.internal` is named by a signature here.
 *
 * @see
 *   [[dsl]] for the combinators alone, when a call site wants authoring without the AST cases in scope.
 */
object MD:

  // ── The trees ──────────────────────────────────────────────────────────────────────────────────────────────

  /** [[MdNode]]'s cases and content-category unions, unprefixed: `Root`, `Paragraph`, `Heading`, `Text`, `Link`, … */
  export MdNode.*

  /** The AST's own type, for a signature that takes a node rather than a case: `def render(node: MdNode)`. */
  export morphir.langkit.markdown.MdNode

  /**
   * The CST's type — the type and its companion only, never its cases.
   *
   * `MdCstNode` names a `Paragraph`, a `Heading` and a `Text` of its own, and every one of them collides with the AST
   * case of the same name that `MD.*` already carries. So the CST's cases stay behind their companion and are reached
   * qualified, as `MdCstNode.Document`; flattening them would make `Paragraph` ambiguous in every file that imports
   * this object.
   */
  export morphir.langkit.markdown.MdCstNode

  // ── Authoring ──────────────────────────────────────────────────────────────────────────────────────────────
  // The combinators, and the String-to-Text conversion that lets a literal stand in for a text node. The given
  // needs `import MD.given` at the call site: a Scala 3 wildcard import does not carry givens.
  export dsl.*
  export dsl.given

  /**
   * Source to tree: `parser.parse(source)` reads under the [[Profile]] in scope and fails with an [[MdParseError]]
   * rather than throwing.
   *
   * A namespace rather than a re-export of the parser itself. `export internal.Parser as parser` would make a public
   * member whose type is `private[markdown]`, which Scala rejects as escaping its defining scope; exporting the
   * *members* is fine, because each generated forwarder's signature names only public types.
   */
  object parser:
    export morphir.langkit.markdown.internal.Parser.*

  /**
   * Tree to source: `writer.write(tree)` spells the document in the [[Style]] in scope, and `writer.raise(tree)` hands
   * back the CST of what `write` would have spelled.
   *
   * A namespace for the same reason [[parser]] is one.
   */
  object writer:
    export morphir.langkit.markdown.internal.MdWriter.*

  // ── The vocabulary the verbs read ──────────────────────────────────────────────────────────────────────────
  // Renamed on the way in: `MD` is the namespace, so the prefix comes off and it is `MD.Style`, not `MD.MdStyle`.
  // One export clause carries both namespaces of a name — the type and its companion — which neither a type
  // alias nor a companion forwarder does alone.
  //
  // Two kinds of name keep their prefix, and for the same reason: it is load-bearing rather than decorative.
  // `MdNode` and `MdCstNode` are two trees that have to be told apart in one scope, and `MdParseError` earns its
  // prefix from outside this module — it is what stops it colliding with `morphir.langkit.elm`'s own parse error
  // in a file that wildcard-imports both.
  export morphir.langkit.markdown.{
    MdProfile as Profile,
    MdStyle as Style,
    MdMeta as Meta,
    MdStyleKeys as StyleKeys,
    FrontMatterKind,
    HeadingStyle,
    HardBreakStyle,
    MetaKey,
    FenceInfo,
    MdParseError
  }

  /** Which of CommonMark's four link spellings an [[MdCstNode.Link]] or [[MdCstNode.Image]] used. */
  export morphir.langkit.markdown.LinkForm

  /**
   * The three opaque types, aliased by hand rather than exported.
   *
   * An `export` of an opaque type's companion re-roots the type of whatever that companion builds at this object, and
   * an extension method defined on the opaque type then falls outside the implicit scope of the result:
   * `YamlDocText("k: v").unwrap` stops resolving, while the same value bound to an ascribed `val` still does. Writing
   * the alias and the companion forwarder out, with the forwarder ascribed at the companion's own singleton type, keeps
   * the original prefix and with it `unwrap` and `toInt`.
   */
  type HeadingLevel = morphir.langkit.markdown.HeadingLevel
  val HeadingLevel: morphir.langkit.markdown.HeadingLevel.type = morphir.langkit.markdown.HeadingLevel

  type ListStart = morphir.langkit.markdown.ListStart
  val ListStart: morphir.langkit.markdown.ListStart.type = morphir.langkit.markdown.ListStart

  type YamlDocText = morphir.langkit.markdown.YamlDocText
  val YamlDocText: morphir.langkit.markdown.YamlDocText.type = morphir.langkit.markdown.YamlDocText

  /** The fold every render target implements, and the traversal it reuses. */
  export morphir.langkit.markdown.Compiler

  /**
   * The verbs over the concrete syntax tree: what the source said, not what it meant.
   *
   * Four methods, which is every verb the CST has, and the only way to them. The objects behind them —
   * `internal.CstParser`, `internal.Cst` and `internal.Lower` — are `private[markdown]` machinery for the same reason
   * [[parser]]'s and [[writer]]'s are, and each is a single-purpose object of one or two methods, so a namespace per
   * object would buy nothing. [[MdCstNode]] itself is flat on [[MD]], with the rest of the types: it is a type a
   * caller's signature names, where these are operations a caller only performs.
   *
   * `cst.parse` and [[parser.parse]] are both here on purpose, and are not two spellings of one thing: each is named
   * for what it produces. `cst.parse` yields an [[MdCstNode.Document]] — every byte of the source, tokens and all — and
   * `parser.parse` yields an [[MdNode.Root]], which is that document lowered to its meaning. Reach for the CST when the
   * question is about syntax, the AST when it is about content.
   */
  object cst:

    /** Source to CST. Total: a source the parser rejects degrades to one verbatim leaf. */
    export morphir.langkit.markdown.internal.CstParser.parse

    /** A tree back to source, byte for byte, and the leaf-tiling invariant that makes that exact. */
    export morphir.langkit.markdown.internal.Cst.{print, tilingErrors}

    /** CST to AST — `MdCstNode.Document => MdNode.Root`, total. */
    export morphir.langkit.markdown.internal.Lower.lower
  end cst
end MD
