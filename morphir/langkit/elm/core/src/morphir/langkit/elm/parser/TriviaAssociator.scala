package morphir.langkit.elm.parser

import morphir.langkit.elm.cst.*

/**
 * Post-processing pass that distributes doc comments from the flat scanned list into the trivia of the module and
 * individual declarations based on source position proximity.
 *
 * After comment scanning, all comments live in `CstModule.trivia`. This pass moves each doc comment to the declaration
 * it precedes (or to the module if it sits between the module declaration and the first import/declaration). Non-doc
 * comments remain in the module-level trivia.
 */
object TriviaAssociator:

  /** Associate doc comments with their target declarations or the module. */
  def associate(module: CstModule): CstModule =
    val allComments                   = module.trivia.comments
    val (docComments, nonDocComments) =
      allComments.partition(_.kind == CommentKind.Doc)

    if docComments.isEmpty then return module

    val sortedDocs = docComments.sortBy(_.span.offset)
    val used       = Array.fill(sortedDocs.size)(false)

    // Determine the boundary after which declarations begin.
    // Doc comments before the first import or declaration (but after the module decl) are module doc comments.
    val firstContentOffset: Int =
      val importStart = module.imports.headOption.map(_.span.offset)
      val declStart   = module.declarations.headOption.map(_.span.offset)
      (importStart ++ declStart).minOption.getOrElse(Int.MaxValue)

    // Associate doc comments with declarations.
    // For each declaration, find the closest preceding unassigned doc comment
    // that ends before the declaration starts and starts after the previous declaration/import ends.
    // Note: Parsley lexer spans include trailing whitespace, so we cannot use span.end as a tight
    // lower bound. Instead we use span.offset of the preceding element as the boundary — a doc
    // comment must start after the *start* of the previous declaration (or import).
    val updatedDecls = module.declarations.zipWithIndex.map { (decl, declIdx) =>
      val declStart  = decl.span.offset
      val lowerBound =
        if declIdx > 0 then module.declarations(declIdx - 1).span.offset
        else module.imports.lastOption.map(_.span.offset).getOrElse(0)

      // Find the closest preceding doc comment in the valid range
      val candidate = sortedDocs.zipWithIndex.reverseIterator.find { case (doc, idx) =>
        !used(idx) &&
        doc.span.end <= declStart &&
        doc.span.offset >= lowerBound
      }

      candidate match
        case Some((doc, idx)) =>
          used(idx) = true
          withTrivia(decl, CstTrivia(IndexedSeq(doc)))
        case None => decl
    }

    // Identify module doc comment: an unassigned doc comment between the module declaration and the
    // first import/declaration. We use moduleDecl.span.offset as the lower bound instead of span.end
    // because the lexer includes trailing whitespace in the span, inflating the end offset past
    // comments that logically follow the module header.
    val moduleDeclOffset = module.moduleDecl.span.offset
    val moduleDocComment = sortedDocs.zipWithIndex.find { case (doc, idx) =>
      !used(idx) &&
      doc.span.offset >= moduleDeclOffset &&
      doc.span.end <= firstContentOffset
    }
    moduleDocComment.foreach { case (_, idx) => used(idx) = true }

    // Build module trivia: module doc comment (if any) + all non-doc comments + any unassigned doc comments
    val unassignedDocs                               = sortedDocs.zip(used).collect { case (doc, false) => doc }
    val moduleTriviaItems: IndexedSeq[CstTriviaItem] =
      moduleDocComment.map(_._1).toIndexedSeq ++ nonDocComments ++ unassignedDocs

    CstModule(
      module.moduleDecl,
      module.imports,
      updatedDecls,
      CstTrivia(moduleTriviaItems)
    )(module.span)

  /** Attach trivia to a declaration, preserving the span from the second parameter list. */
  private def withTrivia(decl: CstDeclaration, trivia: CstTrivia): CstDeclaration = decl match
    case d: CstValueDeclaration      => CstValueDeclaration(d.annotation, d.name, d.patterns, d.body, trivia)(d.span)
    case d: CstTypeAliasDeclaration  => CstTypeAliasDeclaration(d.name, d.typeVariables, d.body, trivia)(d.span)
    case d: CstCustomTypeDeclaration =>
      CstCustomTypeDeclaration(d.name, d.typeVariables, d.constructors, trivia)(d.span)
    case d: CstPortDeclaration  => CstPortDeclaration(d.name, d.typeExpr, trivia)(d.span)
    case d: CstInfixDeclaration =>
      CstInfixDeclaration(d.associativity, d.precedence, d.operator, d.function, trivia)(d.span)
