package morphir.langkit.elm.ast

import morphir.langkit.trees.FieldName
import morphir.langkit.trees.NodeTypeName
import morphir.langkit.trees.QueryableTree

/**
 * QueryableTree instance for the Elm AST.
 *
 * Mirror of [[morphir.langkit.elm.cst.CstQueryableTree]] for the lowered AST. nodeType is the Scala case-class simple
 * name; children delegates to [[AstVisitor.children]]; fields expose the same sub-trees under their case-class field
 * names; text returns the primary identifying string for literals, qualified names, and declarations/refs whose names
 * are raw Strings in the AST (no wrapper node).
 */
object AstQueryableTree:

  given queryableTree: QueryableTree[AstNode] with

    def nodeType(t: AstNode): NodeTypeName =
      // Case-class simple names are guaranteed non-empty and non-whitespace.
      NodeTypeName.make(t.getClass.getSimpleName).toOption.get

    def children(t: AstNode): Seq[AstNode] = AstVisitor.children(t)

    def fields(t: AstNode): Map[FieldName, Seq[AstNode]] =
      // rawFields' keys are compile-time-constant case-class field names — always valid FieldNames.
      rawFields(t).iterator.map((k, v) => FieldName.unsafeMake(k) -> v).toMap

    private def rawFields(t: AstNode): Map[String, Seq[AstNode]] = t match
      case n: Module =>
        Map(
          "exposing"     -> Seq(n.exposing),
          "manager"      -> n.manager.toSeq,
          "imports"      -> n.imports.toSeq,
          "declarations" -> n.declarations.toSeq
        )
      case _: EffectManager => Map.empty
      case _: QualifiedName => Map.empty
      case n: Import        =>
        Map("exposing" -> n.exposing.toSeq)
      case _: ExposingAll                                        => Map.empty
      case n: ExposingExplicit                                   => Map("items" -> n.items)
      case _: ExposedValue | _: ExposedOperator | _: ExposedType => Map.empty
      case n: ValueDeclaration                                   =>
        Map(
          "typeAnnotation" -> n.typeAnnotation.toSeq,
          "parameters"     -> n.parameters.toSeq,
          "body"           -> Seq(n.body)
        )
      case n: TypeAliasDeclaration =>
        Map("body" -> Seq(n.body))
      case n: CustomTypeDeclaration =>
        Map("constructors" -> n.constructors.toSeq)
      case n: PortDeclaration =>
        Map("typeExpr" -> Seq(n.typeExpr))
      case _: InfixDeclaration => Map.empty
      case n: Constructor      => Map("parameters" -> n.parameters.toSeq)
      case n: TypeReference    => Map("name" -> Seq(n.name))
      case _: TypeVariable     => Map.empty
      case n: TypeApplication  =>
        Map("constructor" -> Seq(n.constructor), "arguments" -> n.arguments)
      case n: FunctionType    => Map("from" -> Seq(n.from), "to" -> Seq(n.to))
      case n: TupleType       => Map("elements" -> n.elements)
      case _: UnitType        => Map.empty
      case n: RecordType      => Map("fields" -> n.fields)
      case n: RecordFieldType => Map("typeExpr" -> Seq(n.typeExpr))
      case _: IntLiteral | _: FloatLiteral | _: StringLiteral | _: CharLiteral =>
        Map.empty
      case n: VariableRef         => Map("name" -> Seq(n.name))
      case n: ConstructorRef      => Map("name" -> Seq(n.name))
      case _: OperatorRef         => Map.empty
      case n: FunctionApplication =>
        Map("function" -> Seq(n.function), "arguments" -> n.arguments)
      case n: BinaryOp =>
        Map("left" -> Seq(n.left), "right" -> Seq(n.right))
      case n: Negate     => Map("expr" -> Seq(n.expr))
      case n: IfThenElse =>
        Map(
          "condition"  -> Seq(n.condition),
          "thenBranch" -> Seq(n.thenBranch),
          "elseBranch" -> Seq(n.elseBranch)
        )
      case n: LetIn =>
        Map("bindings" -> n.bindings, "body" -> Seq(n.body))
      case n: LetBinding =>
        Map(
          "typeAnnotation" -> n.typeAnnotation.toSeq,
          "parameters"     -> n.parameters,
          "body"           -> Seq(n.body)
        )
      case n: CaseOf              => Map("expr" -> Seq(n.expr), "branches" -> n.branches)
      case n: CaseBranch          => Map("pattern" -> Seq(n.pattern), "body" -> Seq(n.body))
      case n: Lambda              => Map("parameters" -> n.parameters, "body" -> Seq(n.body))
      case n: TupleLiteral        => Map("elements" -> n.elements)
      case _: UnitLiteral         => Map.empty
      case n: ListLiteral         => Map("elements" -> n.elements)
      case n: RecordLiteral       => Map("fields" -> n.fields)
      case n: RecordField         => Map("value" -> Seq(n.value))
      case n: RecordUpdate        => Map("fields" -> n.fields)
      case n: FieldAccess         => Map("record" -> Seq(n.record))
      case _: FieldAccessFunction => Map.empty
      case n: Parenthesized       => Map("expr" -> Seq(n.expr))
      case _: Glsl                => Map.empty
      case _: AnythingPattern | _: IntPattern | _: FloatPattern | _: StringPattern | _: CharPattern |
          _: VariablePattern | _: UnitPattern =>
        Map.empty
      case n: ConstructorPattern => Map("arguments" -> n.arguments)
      case n: TuplePattern       => Map("elements" -> n.elements)
      case n: ListPattern        => Map("elements" -> n.elements)
      case n: ConsPattern        => Map("head" -> Seq(n.head), "tail" -> Seq(n.tail))
      case _: RecordPattern      => Map.empty
      case n: AsPattern          => Map("pattern" -> Seq(n.pattern))

    def text(t: AstNode): Option[String] = t match
      case n: IntLiteral            => Some(n.value.toString)
      case n: FloatLiteral          => Some(n.value.toString)
      case n: StringLiteral         => Some(n.value)
      case n: CharLiteral           => Some(n.text)
      case n: QualifiedName         => Some(n.fullName)
      case n: ValueDeclaration      => Some(n.name)
      case n: TypeAliasDeclaration  => Some(n.name)
      case n: CustomTypeDeclaration => Some(n.name)
      case n: PortDeclaration       => Some(n.name)
      case n: InfixDeclaration      => Some(n.operator)
      case n: Constructor           => Some(n.name)
      case n: TypeVariable          => Some(n.name)
      case n: OperatorRef           => Some(n.name)
      case n: BinaryOp              => Some(n.operator)
      case n: LetBinding            => Some(n.name)
      case n: RecordField           => Some(n.name)
      case n: RecordUpdate          => Some(n.record)
      case n: FieldAccess           => Some(n.field)
      case n: FieldAccessFunction   => Some(n.field)
      case n: VariablePattern       => Some(n.name)
      case n: AsPattern             => Some(n.alias)
      case n: ExposedValue          => Some(n.name)
      case n: ExposedOperator       => Some(n.name)
      case n: ExposedType           => Some(n.name)
      case _                        => None
