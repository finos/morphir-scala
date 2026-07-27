package morphir.langkit.elm.cst

import morphir.langkit.trees.FieldName
import morphir.langkit.trees.NodeTypeName
import morphir.langkit.trees.QueryableTree

/**
 * QueryableTree instance for the Elm CST.
 *
 * nodeType is the Scala case-class simple name (e.g. "CstValueDeclaration"). children delegates to
 * [[CstVisitor.children]]. fields exposes case-class field names whose values are themselves CstNodes; scalar leaves
 * (String, Long, Double, Char) are surfaced through `text` instead. trivia is excluded from fields to keep queries
 * focused on structure.
 */
object CstQueryableTree:

  given queryableTree: QueryableTree[CstNode] with

    def nodeType(t: CstNode): NodeTypeName =
      // Case-class simple names are guaranteed non-empty and non-whitespace.
      NodeTypeName.make(t.getClass.getSimpleName).toOption.get

    def children(t: CstNode): Seq[CstNode] = CstVisitor.children(t)

    def fields(t: CstNode): Map[FieldName, Seq[CstNode]] =
      // rawFields' keys are compile-time-constant case-class field names — always valid FieldNames.
      rawFields(t).iterator.map((k, v) => FieldName.unsafeMake(k) -> v).toMap

    private def rawFields(t: CstNode): Map[String, Seq[CstNode]] = t match
      case n: CstModule =>
        Map(
          "moduleDecl"   -> Seq(n.moduleDecl),
          "imports"      -> n.imports.toSeq,
          "declarations" -> n.declarations.toSeq
        )
      case n: CstModuleDeclaration =>
        Map("name" -> Seq(n.name), "exposing" -> Seq(n.exposing))
      case n: CstQualifiedName =>
        Map("parts" -> n.parts)
      case _: CstName | _: CstComment =>
        Map.empty
      case n: CstImport =>
        Map(
          "moduleName" -> Seq(n.moduleName),
          "alias"      -> n.alias.toSeq,
          "exposing"   -> n.exposing.toSeq
        )
      case _: CstExposingAll      => Map.empty
      case n: CstExposingExplicit =>
        Map("items" -> n.items)
      case n: CstExposedValue    => Map("name" -> Seq(n.name))
      case n: CstExposedOperator => Map("name" -> Seq(n.name))
      case n: CstExposedType     =>
        Map("name" -> Seq(n.name), "constructors" -> n.constructors.toSeq)
      case _: CstExposedConstructorsAll | _: CstExposedConstructorsNone =>
        Map.empty
      case n: CstValueDeclaration =>
        Map(
          "annotation" -> n.annotation.toSeq,
          "name"       -> Seq(n.name),
          "patterns"   -> n.patterns.toSeq,
          "body"       -> Seq(n.body)
        )
      case n: CstTypeAnnotation =>
        Map("name" -> Seq(n.name), "typeExpr" -> Seq(n.typeExpr))
      case n: CstTypeAliasDeclaration =>
        Map(
          "name"          -> Seq(n.name),
          "typeVariables" -> n.typeVariables.toSeq,
          "body"          -> Seq(n.body)
        )
      case n: CstCustomTypeDeclaration =>
        Map(
          "name"          -> Seq(n.name),
          "typeVariables" -> n.typeVariables.toSeq,
          "constructors"  -> n.constructors.toSeq
        )
      case n: CstConstructor =>
        Map("name" -> Seq(n.name), "parameters" -> n.parameters.toSeq)
      case n: CstPortDeclaration =>
        Map("name" -> Seq(n.name), "typeExpr" -> Seq(n.typeExpr))
      case n: CstInfixDeclaration =>
        Map("operator" -> Seq(n.operator), "function" -> Seq(n.function))
      case n: CstTypeReference   => Map("name" -> Seq(n.name))
      case n: CstTypeVariable    => Map("name" -> Seq(n.name))
      case n: CstTypeApplication =>
        Map("constructor" -> Seq(n.constructor), "arguments" -> n.arguments)
      case n: CstFunctionType =>
        Map("from" -> Seq(n.from), "to" -> Seq(n.to))
      case n: CstTupleType =>
        Map("elements" -> n.elements)
      case _: CstUnitType =>
        Map.empty
      case n: CstRecordType =>
        Map(
          "fields"            -> n.fields,
          "extensionVariable" -> n.extensionVariable.toSeq
        )
      case n: CstRecordFieldType =>
        Map("name" -> Seq(n.name), "typeExpr" -> Seq(n.typeExpr))
      case _: CstIntLiteral | _: CstFloatLiteral | _: CstStringLiteral | _: CstCharLiteral =>
        Map.empty
      case n: CstVariableRef         => Map("name" -> Seq(n.name))
      case n: CstConstructorRef      => Map("name" -> Seq(n.name))
      case n: CstOperatorRef         => Map("name" -> Seq(n.name))
      case n: CstFunctionApplication =>
        Map("function" -> Seq(n.function), "arguments" -> n.arguments)
      case n: CstBinaryOp =>
        Map("left" -> Seq(n.left), "operator" -> Seq(n.operator), "right" -> Seq(n.right))
      case n: CstNegate =>
        Map("expr" -> Seq(n.expr))
      case n: CstIfThenElse =>
        Map(
          "condition"  -> Seq(n.condition),
          "thenBranch" -> Seq(n.thenBranch),
          "elseBranch" -> Seq(n.elseBranch)
        )
      case n: CstLetIn =>
        Map("bindings" -> n.bindings, "body" -> Seq(n.body))
      case n: CstLetBinding =>
        Map(
          "annotation" -> n.annotation.toSeq,
          "pattern"    -> Seq(n.pattern),
          "parameters" -> n.parameters,
          "body"       -> Seq(n.body)
        )
      case n: CstCaseOf =>
        Map("expr" -> Seq(n.expr), "branches" -> n.branches)
      case n: CstCaseBranch =>
        Map("pattern" -> Seq(n.pattern), "body" -> Seq(n.body))
      case n: CstLambda =>
        Map("parameters" -> n.parameters, "body" -> Seq(n.body))
      case n: CstTupleLiteral =>
        Map("elements" -> n.elements)
      case _: CstUnitLiteral =>
        Map.empty
      case n: CstListLiteral =>
        Map("elements" -> n.elements)
      case n: CstRecordLiteral =>
        Map("fields" -> n.fields)
      case n: CstRecordField =>
        Map("name" -> Seq(n.name), "value" -> Seq(n.value))
      case n: CstRecordUpdate =>
        Map("record" -> Seq(n.record), "fields" -> n.fields)
      case n: CstFieldAccess =>
        Map("record" -> Seq(n.record), "field" -> Seq(n.field))
      case n: CstFieldAccessFunction =>
        Map("field" -> Seq(n.field))
      case n: CstParenthesized =>
        Map("expr" -> Seq(n.expr))
      case _: CstGlsl =>
        Map.empty
      case _: CstAnythingPattern | _: CstIntPattern | _: CstFloatPattern | _: CstStringPattern |
          _: CstCharPattern | _: CstUnitPattern =>
        Map.empty
      case n: CstVariablePattern    => Map("name" -> Seq(n.name))
      case n: CstConstructorPattern =>
        Map("name" -> Seq(n.name), "arguments" -> n.arguments)
      case n: CstTuplePattern =>
        Map("elements" -> n.elements)
      case n: CstListPattern =>
        Map("elements" -> n.elements)
      case n: CstConsPattern =>
        Map("head" -> Seq(n.head), "tail" -> Seq(n.tail))
      case n: CstRecordPattern =>
        Map("fields" -> n.fields)
      case n: CstAsPattern =>
        Map("pattern" -> Seq(n.pattern), "alias" -> Seq(n.alias))
      case n: CstParenthesizedPattern =>
        Map("pattern" -> Seq(n.pattern))

    def text(t: CstNode): Option[String] = t match
      case n: CstName          => Some(n.value)
      case n: CstIntLiteral    => Some(n.value.toString)
      case n: CstFloatLiteral  => Some(n.value.toString)
      case n: CstStringLiteral => Some(n.value)
      case n: CstCharLiteral   => Some(n.value.toString)
      case n: CstComment       => Some(n.text)
      case _                   => None
