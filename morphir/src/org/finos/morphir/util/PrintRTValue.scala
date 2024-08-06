package org.finos.morphir.util

import fansi.Str
import pprint.{Renderer, Tree, Truncated}
import _root_.org.finos.morphir.naming.*
import java.util.function.UnaryOperator
import org.finos.morphir.ir.printing.PrintIR
import org.finos.morphir.runtime.RTValue as RT

object PrintRTValue {
  def apply(
      any: Any,
      detailLevel: PrintRTValue.DetailLevel = DetailLevel.BirdsEye,
      fieldNames: FieldNames = FieldNames.Hide,
      defaultWidth: Int = 150
  ) = new PrintRTValue(detailLevel, fieldNames, defaultWidth).apply(any)

  sealed trait DetailLevel {
    def hideFQNames: Boolean
  }
  object DetailLevel {
    object Detailed extends DetailLevel {
      def hideFQNames = false
      // def compressNestedConcepts = false
    }
    object Medium extends DetailLevel {
      def hideFQNames = true
    }
    object BirdsEye extends DetailLevel {
      def hideFQNames = true
    }
  }
}

class PrintRTValue(
    detailLevel: PrintRTValue.DetailLevel,
    fieldNames: FieldNames,
    val defaultWidth: Int
) extends pprint.Walker {
  val showFieldNames                = fieldNames == FieldNames.Show
  val escapeUnicode                 = false
  val defaultHeight: Int            = Integer.MAX_VALUE
  val defaultIndent: Int            = 2
  val colorLiteral: fansi.Attrs     = fansi.Color.Green
  val colorApplyPrefix: fansi.Attrs = fansi.Color.Yellow
  var verbose: Boolean              = false

  override def additionalHandlers: PartialFunction[Any, Tree] = PartialFunction.empty

  def apply(x: Any, verbose: Boolean = false): fansi.Str = {
    this.verbose = verbose
    val tokenized = this.tokenize(x).toSeq
    fansi.Str.join(tokenized)
  }

  def treeify(x: Any): Tree = this.treeify(x, escapeUnicode, showFieldNames)
  def treeifySuper(x: Any): Tree =
    super.treeify(x, escapeUnicode, showFieldNames)

  override def treeify(x: Any, escapeUnicode: Boolean, showFieldNames: Boolean): Tree = x match {

    case name: Name =>
      Tree.Literal(name.toTitleCase)

    case qn: FQName =>
      if (detailLevel.hideFQNames)
        Tree.Literal(qn.localName.toTitleCase)
      else
        Tree.Literal(qn.toStringTitleCase)

    case rt: RT => PrintRTInner.of(rt)

    case other => super.treeify(other, escapeUnicode, showFieldNames)
  }

  object PrintRTInner {
    def of(v: RT): Tree =
      v match {
        case v: RT.Primitive.Int =>
          val mInt = v.value
          if (mInt.isValidInt) {
            Tree.Literal(mInt.toInt.toString)
          } else {
            Tree.Literal(mInt.toString)
          }
        case v: RT.Primitive.String =>
          Tree.Literal(s""""${v.value}"""")
        case v: RT.Primitive.Char =>
          Tree.Literal(s"""'${v.value}'""")
        case v: RT.Primitive[_] => Tree.Literal(v.value.toString)
        case v: RT.LocalDate    => Tree.Literal(v.value.toString)
        case v: RT.LocalTime    => Tree.Literal(v.value.toString)
        case v: RT.Unit         => Tree.ofRT(v)(List())

        case v: RT.Tuple =>
          Tree.ofRT(v)(v.elements.map(treeify(_)))

        case v: RT.Record =>
          val body = v.elements.map { case (k, v) => Tree.KeyValue(k.toCamelCase, treeify(v)) }.toList
          Tree.ofRT(v)(body)

        case v: RT.List =>
          Tree.ofRT(v)(v.elements.map(r => treeify(r)))

        case v: RT.Map =>
          val body = v.elements.map { case (k, v) => Tree.Infix(treeify(k), "->", treeify(v)) }.toList
          Tree.ofRT(v)(body)

        case v: RT.Set =>
          Tree.ofRT(v)(v.elements.toList.map(r => treeify(r)))

        case v: RT.Aggregation =>
          Tree.Literal(v.value.toString)

        case key: RT.Key => key match {
            case RT.Key0   => Tree.Literal("0")
            case v: RT.Key => Tree.ofRT(v)(v.value.map(treeify(_)))
          }

        case v: RT.Function =>
          val body = v match {
            case RT.FieldFunction(name) =>
              List(Tree.Literal(s".${name.toCamelCase}"))
            case RT.LambdaFunction(body, pattern, _, _) =>
              List(Tree.Infix(Tree.Literal(s"\\${pattern.toString}"), "->", Tree.Literal(body.toString)))
            case RT.DefinitionFunction(body, arguments, curried, _, loc) =>
              val paramsTree = Tree.KeyValue(
                "parameters",
                Tree.Apply(
                  "",
                  arguments.map(arg => Tree.Literal(s"${arg._1.toCamelCase} : ${arg._2.toString}")).iterator
                )
              )
              val curriedTree = Tree.KeyValue(
                "curried",
                Tree.Apply("", curried.map(arg => Tree.KeyValue(arg._1.toCamelCase, treeify(arg._2))).iterator)
              )
              val bodyTree  = Tree.KeyValue("body", Tree.Literal(body.toString))
              val locString = Tree.Literal(loc.toString)
              if (curried.length > 0) {
                List(locString, paramsTree, curriedTree, bodyTree)
              } else {
                List(locString, paramsTree, bodyTree)
              }
            case RT.ConstructorFunction(name, arguments, curried) =>
              val paramsTree =
                Tree.KeyValue("parameters", Tree.Apply("", arguments.map(arg => Tree.Literal(arg.toString)).iterator))
              val curriedTree = Tree.KeyValue("curried", Tree.Apply("", curried.map(arg => treeify(arg)).iterator))
              val nameTree    = Tree.KeyValue("name", treeify(name))
              if (curried.length > 0) {
                List(paramsTree, curriedTree, nameTree)
              } else {
                List(paramsTree, nameTree)
              }
            case RT.ImplicitConstructorFunction(name, arguments, curried) =>
              val paramsTree =
                Tree.KeyValue("parameters", Tree.Apply("", arguments.map(arg => Tree.Literal(arg.toString)).iterator))
              val curriedTree = Tree.KeyValue("curried", Tree.Apply("", curried.map(arg => treeify(arg)).iterator))
              val nameTree    = Tree.KeyValue("name", treeify(name))
              if (curried.size > 0) {
                List(paramsTree, curriedTree, nameTree)
              } else {
                List(paramsTree, nameTree)
              }

            case RT.NativeFunction(argCount, curried, signature, loc) =>
              val locString     = Tree.Literal(loc.toString)
              val argCountTree  = Tree.KeyValue("remaining args", treeify(argCount))
              val totalArgsTree = Tree.KeyValue("expected args", treeify(signature.numArgs))
              val curriedTree   = Tree.KeyValue("curried", Tree.Apply("", curried.map(arg => treeify(arg)).iterator))
              if (curried.length > 0) {
                List(locString, argCountTree, totalArgsTree, curriedTree)
              } else {
                List(locString, totalArgsTree)
              }
            case RT.NativeInnerFunction(argCount, curried, signature, loc) =>
              val locString     = Tree.Literal(loc.toString)
              val argCountTree  = Tree.KeyValue("remaining args", treeify(argCount))
              val totalArgsTree = Tree.KeyValue("expected args", treeify(signature.numArgs))
              val curriedTree   = Tree.KeyValue("curried", Tree.Apply("", curried.map(arg => treeify(arg)).iterator))
              if (curried.length > 0) {
                List(locString, argCountTree, totalArgsTree, curriedTree)
              } else {
                List(locString, argCountTree, totalArgsTree)
              }

          }
          Tree.ofRT(v)(
            body
          )

        case v: RT.ConstructorResult =>
          val fqnString = if (detailLevel.hideFQNames)
            v.name.localName.toTitleCase
          else
            v.name.toStringTitleCase
          Tree.Apply(fqnString, v.values.map(treeify(_)).iterator)
      }
  }

  implicit class TreeOpts(tree: Tree.type) {
    def ofRT(d: RT)(children: List[Tree]) =
      Tree.Apply(d.printName, children.iterator)

  }

  implicit class RTPrintOpts(v: RT) {
    def printName: String =
      v.getClass.getSimpleName

  }

  def tokenize(x: Any): Iterator[fansi.Str] = {
    val tree      = this.treeify(x)
    val renderer  = new Renderer(defaultWidth, colorApplyPrefix, colorLiteral, defaultIndent)
    val rendered  = renderer.rec(tree, 0, 0).iter
    val truncated = new Truncated(rendered, defaultWidth, defaultHeight)
    truncated
  }
}
