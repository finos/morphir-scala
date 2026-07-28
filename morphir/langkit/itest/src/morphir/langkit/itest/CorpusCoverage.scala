package morphir.langkit.itest

import scala.io.Source

import morphir.langkit.elm.Elm
import morphir.langkit.elm.cst.CstNode
import morphir.langkit.elm.cst.CstVisitor.children

/**
 * The conformance corpus, and what it is expected to exercise.
 *
 * The corpus exists because the fixtures it joins did not catch real bugs: eight modules, some two hundred lines, one
 * `let` (annotated, which is why an un-annotated `let` binding failing to parse at all went unnoticed), no `case`, no
 * lambda, no operator. A suite can only be evidence for what it covers.
 *
 * [[requiredNodeTypes]] is the standard the corpus is held to. A node type on that list with no instance anywhere in
 * the corpus is either an untested construct or one the parser stopped producing — `CstOperatorRef` was the second
 * kind, modelled and lowered but never parsed, until this list asked for it.
 */
object CorpusCoverage:

  /** Corpus modules, as classpath resources. */
  val modules: List[String] = List(
    "fixtures/conformance/Expressions.elm",
    "fixtures/conformance/Patterns.elm",
    "fixtures/conformance/Types.elm",
    "fixtures/conformance/Layout.elm",
    "fixtures/conformance/Operators.elm",
    "fixtures/conformance/ModuleForms.elm",
    "fixtures/conformance/Comments.elm",
    "fixtures/conformance/Shaders.elm"
  )

  /**
   * Every CST node type the corpus must contain an instance of.
   *
   * Deliberately a written list rather than something derived: adding a node type should be a decision to cover it, and
   * the failure message names exactly what is missing.
   *
   * The list is meant to be complete: every node type the CST models should appear in it, and every one of those should
   * have an instance somewhere in the corpus.
   */
  val requiredNodeTypes: Set[String] = Set(
    // module structure
    "CstModule",
    "CstModuleDeclaration",
    "CstEffectManager",
    "CstQualifiedName",
    "CstName",
    "CstImport",
    "CstComment",
    "CstExposingAll",
    "CstExposingExplicit",
    "CstExposedValue",
    "CstExposedType",
    "CstExposedConstructorsAll",
    // declarations
    "CstValueDeclaration",
    "CstTypeAnnotation",
    "CstTypeAliasDeclaration",
    "CstCustomTypeDeclaration",
    "CstConstructor",
    "CstPortDeclaration",
    "CstInfixDeclaration",
    // type expressions
    "CstTypeReference",
    "CstTypeVariable",
    "CstTypeApplication",
    "CstFunctionType",
    "CstTupleType",
    "CstUnitType",
    "CstRecordType",
    "CstRecordFieldType",
    // expressions
    "CstIntLiteral",
    "CstFloatLiteral",
    "CstStringLiteral",
    "CstCharLiteral",
    "CstVariableRef",
    "CstConstructorRef",
    "CstOperatorRef",
    "CstFunctionApplication",
    "CstBinaryOp",
    "CstNegate",
    "CstIfThenElse",
    "CstLetIn",
    "CstLetBinding",
    "CstCaseOf",
    "CstCaseBranch",
    "CstLambda",
    "CstTupleLiteral",
    "CstUnitLiteral",
    "CstListLiteral",
    "CstRecordLiteral",
    "CstRecordField",
    "CstRecordUpdate",
    "CstFieldAccess",
    "CstFieldAccessFunction",
    "CstParenthesized",
    "CstGlsl",
    // patterns
    "CstAnythingPattern",
    "CstIntPattern",
    "CstFloatPattern",
    "CstStringPattern",
    "CstCharPattern",
    "CstVariablePattern",
    "CstUnitPattern",
    "CstConstructorPattern",
    "CstTuplePattern",
    "CstListPattern",
    "CstConsPattern",
    "CstRecordPattern",
    "CstAsPattern",
    "CstParenthesizedPattern"
  )

  /** Read a corpus module from the classpath. */
  def read(resource: String): String =
    val stream = Option(getClass.getClassLoader.getResourceAsStream(resource)).getOrElse(
      throw new AssertionError(s"corpus module not found on the classpath: $resource")
    )
    try Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()

  /** Every node type appearing in `node`, by its `nodeType` name. */
  def nodeTypes(node: CstNode): Set[String] =
    children(node).foldLeft(Set(node.getClass.getSimpleName))((seen, child) => seen ++ nodeTypes(child))

  /** Parse every corpus module, failing loudly on any that does not parse. */
  def parseAll(): Map[String, CstNode] =
    modules.map { resource =>
      val source = read(resource)
      val module = Elm.parseCst(source).fold(
        diagnostic => throw new AssertionError(s"corpus module failed to parse: $resource\n${diagnostic.message}"),
        identity
      )
      resource -> (module: CstNode)
    }.toMap

  /** Every node type the corpus actually contains. */
  def coveredNodeTypes(): Set[String] =
    parseAll().values.foldLeft(Set.empty[String])((seen, module) => seen ++ nodeTypes(module))

  /** Node types required but absent from the whole corpus. */
  def missingNodeTypes(): Set[String] =
    requiredNodeTypes.diff(coveredNodeTypes())

  /**
   * Print what the corpus covers, for someone adding a module or a node type.
   *
   * `./mill morphir.langkit.itest.runMain morphir.langkit.itest.CorpusCoverage`
   */
  def main(args: Array[String]): Unit =
    val covered = coveredNodeTypes()
    val missing = requiredNodeTypes.diff(covered)
    val extra   = covered.diff(requiredNodeTypes)
    println(s"corpus modules:  ${modules.size}")
    println(s"required types:  ${requiredNodeTypes.size}")
    println(s"covered types:   ${covered.intersect(requiredNodeTypes).size}")
    if missing.nonEmpty then println(s"missing:\n  ${missing.toList.sorted.mkString("\n  ")}")
    if extra.nonEmpty then println(s"covered but not required:\n  ${extra.toList.sorted.mkString("\n  ")}")
