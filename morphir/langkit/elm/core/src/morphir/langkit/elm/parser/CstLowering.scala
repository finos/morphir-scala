package morphir.langkit.elm.parser

import morphir.langkit.core.Span
import morphir.langkit.elm.cst.*
import morphir.langkit.elm.ast

/**
 * Transforms a CST into a simplified AST by discarding syntactic details (parentheses, concrete token positions) and
 * normalizing the tree structure.
 */
object CstLowering:

  def lowerModule(cst: CstModule): ast.Module =
    ast.Module(
      name = lowerQualifiedName(cst.moduleDecl.name),
      exposing = lowerExposingList(cst.moduleDecl.exposing),
      imports = cst.imports.map(lowerImport),
      declarations = cst.declarations.map(lowerDeclaration),
      docComment = cst.trivia.docComment.map(_.text)
    )(cst.span)

  private def lowerQualifiedName(cst: CstQualifiedName): ast.QualifiedName =
    ast.QualifiedName(cst.parts.map(_.value))(cst.span)

  private def lowerExposingList(cst: CstExposingList): ast.ExposingList = cst match
    case n: CstExposingAll      => ast.ExposingAll()(n.span)
    case n: CstExposingExplicit => ast.ExposingExplicit(n.items.map(lowerExposedItem))(n.span)

  private def lowerExposedItem(cst: CstExposedItem): ast.ExposedItem = cst match
    case n: CstExposedValue    => ast.ExposedValue(n.name.value)(n.span)
    case n: CstExposedOperator => ast.ExposedOperator(n.name.value)(n.span)
    case n: CstExposedType     =>
      val exposeCtors = n.constructors.exists(_.isInstanceOf[CstExposedConstructorsAll])
      ast.ExposedType(n.name.value, exposeCtors)(n.span)

  private def lowerImport(cst: CstImport): ast.Import =
    ast.Import(
      moduleName = lowerQualifiedName(cst.moduleName),
      alias = cst.alias.map(_.value),
      exposing = cst.exposing.map(lowerExposingList)
    )(cst.span)

  def lowerDeclaration(cst: CstDeclaration): ast.Declaration = cst match
    case n: CstValueDeclaration =>
      ast.ValueDeclaration(
        name = n.name.value,
        typeAnnotation = n.annotation.map(a => lowerTypeExpression(a.typeExpr)),
        parameters = n.patterns.map(lowerPattern),
        body = lowerExpression(n.body),
        docComment = n.trivia.docComment.map(_.text)
      )(n.span)
    case n: CstTypeAliasDeclaration =>
      ast.TypeAliasDeclaration(
        name = n.name.value,
        typeVariables = n.typeVariables.map(_.value),
        body = lowerTypeExpression(n.body),
        docComment = n.trivia.docComment.map(_.text)
      )(n.span)
    case n: CstCustomTypeDeclaration =>
      ast.CustomTypeDeclaration(
        name = n.name.value,
        typeVariables = n.typeVariables.map(_.value),
        constructors = n.constructors.map(lowerConstructor),
        docComment = n.trivia.docComment.map(_.text)
      )(n.span)
    case n: CstPortDeclaration =>
      ast.PortDeclaration(
        name = n.name.value,
        typeExpr = lowerTypeExpression(n.typeExpr),
        docComment = n.trivia.docComment.map(_.text)
      )(n.span)
    case n: CstInfixDeclaration =>
      ast.InfixDeclaration(
        associativity = lowerAssociativity(n.associativity),
        precedence = n.precedence,
        operator = n.operator.value,
        function = n.function.value,
        docComment = n.trivia.docComment.map(_.text)
      )(n.span)

  private def lowerConstructor(cst: CstConstructor): ast.Constructor =
    ast.Constructor(cst.name.value, cst.parameters.map(lowerTypeExpression))(cst.span)

  private def lowerAssociativity(a: Associativity): ast.Associativity = a match
    case Associativity.Left  => ast.Associativity.Left
    case Associativity.Right => ast.Associativity.Right
    case Associativity.Non   => ast.Associativity.Non

  def lowerTypeExpression(cst: CstTypeExpression): ast.TypeExpression = cst match
    case n: CstTypeReference   => ast.TypeReference(lowerQualifiedName(n.name))(n.span)
    case n: CstTypeVariable    => ast.TypeVariable(n.name.value)(n.span)
    case n: CstTypeApplication =>
      ast.TypeApplication(lowerTypeExpression(n.constructor), n.arguments.map(lowerTypeExpression))(n.span)
    case n: CstFunctionType => ast.FunctionType(lowerTypeExpression(n.from), lowerTypeExpression(n.to))(n.span)
    case n: CstTupleType    => ast.TupleType(n.elements.map(lowerTypeExpression))(n.span)
    case n: CstUnitType     => ast.UnitType()(n.span)
    case n: CstRecordType   =>
      ast.RecordType(
        fields = n.fields.map(f => ast.RecordFieldType(f.name.value, lowerTypeExpression(f.typeExpr))(f.span)),
        extensionVariable = n.extensionVariable.map(_.value)
      )(n.span)

  def lowerExpression(cst: CstExpression): ast.Expression = cst match
    case n: CstIntLiteral          => ast.IntLiteral(n.value)(n.span)
    case n: CstFloatLiteral        => ast.FloatLiteral(n.value)(n.span)
    case n: CstStringLiteral       => ast.StringLiteral(n.value)(n.span)
    case n: CstCharLiteral         => ast.CharLiteral(n.codePoint)(n.span)
    case n: CstVariableRef         => ast.VariableRef(lowerQualifiedName(n.name))(n.span)
    case n: CstConstructorRef      => ast.ConstructorRef(lowerQualifiedName(n.name))(n.span)
    case n: CstOperatorRef         => ast.OperatorRef(n.name.value)(n.span)
    case n: CstFunctionApplication =>
      ast.FunctionApplication(lowerExpression(n.function), n.arguments.map(lowerExpression))(n.span)
    case n: CstBinaryOp =>
      ast.BinaryOp(lowerExpression(n.left), n.operator.value, lowerExpression(n.right))(n.span)
    case n: CstNegate     => ast.Negate(lowerExpression(n.expr))(n.span)
    case n: CstIfThenElse =>
      ast.IfThenElse(
        lowerExpression(n.condition),
        lowerExpression(n.thenBranch),
        lowerExpression(n.elseBranch)
      )(n.span)
    case n: CstLetIn =>
      ast.LetIn(n.bindings.map(lowerLetBinding), lowerExpression(n.body))(n.span)
    case n: CstCaseOf =>
      ast.CaseOf(lowerExpression(n.expr), n.branches.map(lowerCaseBranch))(n.span)
    case n: CstLambda =>
      ast.Lambda(n.parameters.map(lowerPattern), lowerExpression(n.body))(n.span)
    case n: CstTupleLiteral  => ast.TupleLiteral(n.elements.map(lowerExpression))(n.span)
    case n: CstUnitLiteral   => ast.UnitLiteral()(n.span)
    case n: CstListLiteral   => ast.ListLiteral(n.elements.map(lowerExpression))(n.span)
    case n: CstRecordLiteral =>
      ast.RecordLiteral(n.fields.map(f => ast.RecordField(f.name.value, lowerExpression(f.value))(f.span)))(
        n.span
      )
    case n: CstRecordUpdate =>
      ast.RecordUpdate(
        n.record.value,
        n.fields.map(f => ast.RecordField(f.name.value, lowerExpression(f.value))(f.span))
      )(n.span)
    case n: CstFieldAccess         => ast.FieldAccess(lowerExpression(n.record), n.field.value)(n.span)
    case n: CstFieldAccessFunction => ast.FieldAccessFunction(n.field.value)(n.span)
    case n: CstParenthesized       => ast.Parenthesized(lowerExpression(n.expr))(n.span)
    case n: CstGlsl                => ast.Glsl(n.code)(n.span)

  private def lowerLetBinding(cst: CstLetBinding): ast.LetBinding =
    val name = cst.pattern match
      case n: CstVariablePattern => n.name.value
      case _                     => "<pattern>"
    ast.LetBinding(
      name = name,
      typeAnnotation = cst.annotation.map(a => lowerTypeExpression(a.typeExpr)),
      parameters = cst.parameters.map(lowerPattern),
      body = lowerExpression(cst.body)
    )(cst.span)

  private def lowerCaseBranch(cst: CstCaseBranch): ast.CaseBranch =
    ast.CaseBranch(lowerPattern(cst.pattern), lowerExpression(cst.body))(cst.span)

  def lowerPattern(cst: CstPattern): ast.Pattern = cst match
    case n: CstAnythingPattern    => ast.AnythingPattern()(n.span)
    case n: CstIntPattern         => ast.IntPattern(n.value)(n.span)
    case n: CstFloatPattern       => ast.FloatPattern(n.value)(n.span)
    case n: CstStringPattern      => ast.StringPattern(n.value)(n.span)
    case n: CstCharPattern        => ast.CharPattern(n.codePoint)(n.span)
    case n: CstVariablePattern    => ast.VariablePattern(n.name.value)(n.span)
    case n: CstUnitPattern        => ast.UnitPattern()(n.span)
    case n: CstConstructorPattern =>
      ast.ConstructorPattern(lowerQualifiedName(n.name), n.arguments.map(lowerPattern))(n.span)
    case n: CstTuplePattern         => ast.TuplePattern(n.elements.map(lowerPattern))(n.span)
    case n: CstListPattern          => ast.ListPattern(n.elements.map(lowerPattern))(n.span)
    case n: CstConsPattern          => ast.ConsPattern(lowerPattern(n.head), lowerPattern(n.tail))(n.span)
    case n: CstRecordPattern        => ast.RecordPattern(n.fields.map(_.value))(n.span)
    case n: CstAsPattern            => ast.AsPattern(lowerPattern(n.pattern), n.alias.value)(n.span)
    case n: CstParenthesizedPattern => lowerPattern(n.pattern)
