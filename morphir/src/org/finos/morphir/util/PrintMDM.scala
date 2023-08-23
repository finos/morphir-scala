package org.finos.morphir.util

import fansi.Str
import pprint.{Renderer, Tree, Truncated}
import _root_.org.finos.morphir.naming.*
import _root_.org.finos.morphir.datamodel.Label
import _root_.org.finos.morphir.datamodel.EnumLabel
import _root_.org.finos.morphir.datamodel.Data
import _root_.org.finos.morphir.datamodel.Concept
import org.finos.morphir.datamodel.Data.{Optional, Result}

import java.util.function.UnaryOperator

sealed trait DetailLevel {
  def hideFQNames: Boolean
  def compressNestedConcepts: Boolean
  def compressData: Boolean
  def compressConcept: Boolean
  def hideInnerConcepts: Boolean
}
object DetailLevel {
  object Detailed extends DetailLevel {
    def hideFQNames            = false
    def compressNestedConcepts = false
    def compressData           = false
    def compressConcept        = false
    def hideInnerConcepts      = false
  }
  object Medium extends DetailLevel {
    def hideFQNames            = true
    def compressNestedConcepts = false
    def compressData           = false
    def compressConcept        = true
    def hideInnerConcepts      = false
  }
  object BirdsEye extends DetailLevel {
    def hideFQNames            = true
    def compressNestedConcepts = false
    def compressData           = true
    def compressConcept        = true
    def hideInnerConcepts      = true
  }

  object BirdsEye2 extends DetailLevel {
    def hideFQNames            = false
    def compressNestedConcepts = false
    def compressData           = true
    def compressConcept        = true
    def showInnerConcepts      = false
    def hideInnerConcepts      = true
  }

  object BirdsEye3 extends DetailLevel {
    def hideFQNames            = false
    def compressNestedConcepts = false
    def compressData           = true
    def compressConcept        = true
    def showInnerConcepts      = false
    def hideInnerConcepts      = false
  }
}

object PrintMDM {
  def apply(
      any: Any,
      detailLevel: DetailLevel = DetailLevel.BirdsEye,
      fieldNames: FieldNames = FieldNames.Hide,
      defaultWidth: Int = 150
  ) = new PrintIR(detailLevel, fieldNames, defaultWidth).apply(any)
}

sealed trait FieldNames
object FieldNames {
  object Show extends FieldNames
  object Hide extends FieldNames
}

class PrintIR(
    detailLevel: DetailLevel,
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

  def treeify(x: Any): Tree      = this.treeify(x, escapeUnicode, showFieldNames)
  def treeifySuper(x: Any): Tree = super.treeify(x, escapeUnicode, showFieldNames)

  override def treeify(x: Any, escapeUnicode: Boolean, showFieldNames: Boolean): Tree = x match {

    case name: Name =>
      Tree.Literal(name.toTitleCase)

    case qn: FQName =>
      if (detailLevel.hideFQNames)
        Tree.Literal(qn.localName.toTitleCase)
      else
        Tree.Literal(qn.toStringTitleCase)

    case label: Label => Tree.Literal(label.value)

    case v: Data =>
      if (detailLevel.compressData)
        PrintData.of(v)
      else
        appendPrefix(super.treeify(v, escapeUnicode, showFieldNames), "Data.")

    case v: Concept =>
      if (detailLevel.compressConcept)
        PrintConcept.of(v)
      else
        appendPrefix(super.treeify(v, escapeUnicode, showFieldNames), "Concept.")

    case other => super.treeify(other, escapeUnicode, showFieldNames)
  }

  def appendPrefix(tree: Tree, prefix: String) =
    tree match {
      case Tree.Apply(head, body) => Tree.Apply(prefix + head, body)
      case other                  => other
    }

  object PrintData {
    def of(v: Data): Tree =
      v match {
        case v: Data.Basic[_] =>
          v match {
            case v: Data.LocalDate => Tree.Literal(v.toString)
            case v: Data.Month     => Tree.Literal(v.toString)
            case v: Data.LocalTime => Tree.Literal(v.toString)
            case _                 => treeifySuper(v)
          }

        case v: Data.Case =>
          val body = v.values.asInstanceOf[scala.List[(EnumLabel, Data)]].map { case (enumLabel, data) =>
            enumLabel match {
              case EnumLabel.Empty        => Tree.KeyValue("<EMPTY>", treeify(data))
              case EnumLabel.Named(value) => Tree.KeyValue(s"<${value}>", treeify(data))
            }
          }
          Tree.Apply("@" + v.enumLabel + ":Case", (body :+ treeify(v.shape)).iterator)

        case v: Data.Tuple =>
          Tree.ofData(v)(v.values.map(treeify(_)))

        case v: Data.Struct =>
          val body = v.values.map { case (k, v) => Tree.KeyValue(k.value, treeify(v)) }
          Tree.ofData(v)(body)

        case v: Data.Record =>
          val body = v.values.map { case (k, v) => Tree.KeyValue(k.value, treeify(v)) }
          Tree.ofData(v)(body)

        case v: Data.Optional =>
          v match {
            case Optional.Some(data, _) => Tree.Apply(v.printName, List(treeify(data)).iterator)
            case Optional.None(_)       => Tree.Literal(v.printName)
          }

        case v: Data.Result =>
          v match {
            case Result.Ok(data, _)  => Tree.ofData(v)(List(treeify(data)))
            case Result.Err(data, _) => Tree.ofData(v)(List(treeify(data)))
          }

        case v: Data.List =>
          Tree.ofData(v)(v.values.map(r => treeify(r)))

        case v: Data.Map =>
          Tree.ofData(v)(v.values.toList.map(treeify(_)))

        case v: Data.Union =>
          Tree.ofData(v)(List(treeify(v.value)))

        case v: Data.Aliased =>
          Tree.ofData(v)(List(treeify(v.data)))
      }
  }

  object PrintConcept {
    def of(c: Concept) =
      c match {
        case v: Concept.Basic[_] => Tree.Literal(v.printName)
        case v: Concept.Any.type => Tree.Literal(v.printName)

        case v: Concept.Record =>
          val caseNames =
            if (detailLevel.hideInnerConcepts)
              v.fields.map(_._1.value).map(Tree.Literal(_))
            else {
              v.fields.map { case (label, value) => Tree.KeyValue(label.value, treeify(value)) }
            }
          Tree.ofConcept(v)(caseNames)

        case v: Concept.Struct =>
          val caseNames = v.fields.map(_._1.value)
          Tree.ofConcept(v)(caseNames.map(Tree.Literal(_)))

        case v @ Concept.Alias(name, value) =>
          if (detailLevel.hideFQNames)
            Tree.ofConcept(v)(List(treeify(value)))
          else
            Tree.ofConcept(v)(List(treeify(name), treeify(value)))

        case v @ Concept.List(elementType) =>
          Tree.ofConcept(v)(List(treeify(elementType)))

        case v @ Concept.Map(keyType, valueType) =>
          Tree.ofConcept(v)(List(treeify(keyType), treeify(valueType)))

        case v: Concept.Tuple =>
          Tree.ofConcept(v)(v.values.map(treeify(_)))

        case c @ Concept.Optional(elementType) =>
          Tree.ofConcept(c)(List(treeify(elementType)))

        case r @ Concept.Result(errType, okType) =>
          Tree.ofConcept(r)(List(treeify(errType), treeify(okType)))

        case c: Concept.Enum =>
          val caseNames =
            if (detailLevel.hideInnerConcepts)
              c.cases.map(c => c.label.value).map(Tree.Literal(_))
            else {
              c.cases.map { enumCase =>
                val cases =
                  enumCase.fields.map {
                    case (enumLabel, data) =>
                      enumLabel match {
                        case EnumLabel.Empty        => Tree.KeyValue("<EMPTY>", treeify(data))
                        case EnumLabel.Named(value) => Tree.KeyValue(s"<${value}>", treeify(data))
                      }
                  }
                Tree.Apply("$" + enumCase.label.value + ":Enum.Case", cases.iterator)
              }
            }
          Tree.ofConcept(c)(caseNames)

        case c @ Concept.Union(cases) =>
          Tree.ofConcept(c)(cases.map(treeify(_)))
      }
  }

  implicit class TreeOpts(tree: Tree.type) {
    // Don't display full paths even for data-elements that have them,
    // display all of that information in the concept-area
    def ofData(d: Data)(children: List[Tree]) =
      Tree.Apply(d.printName, children.iterator)

    def ofConcept(c: Concept)(children: List[Tree]) =
      if (detailLevel.hideFQNames)
        Tree.Apply(c.printName, children.iterator)
      else {
        val elems = c.getName.map(_.toStringTitleCase).map(treeify(_)).toList ++ children
        c.getName match {
          case Some(value) => Tree.Apply(c.printName, (treeify(value) +: children).iterator)
          case None        => Tree.Apply(c.printName, children.iterator)
        }
      }
  }

  implicit class DataPrintOpts(v: Data) {
    def printName: String =
      v.getNameString match {
        // e.g. @Person:Record(...)
        case Some(value) => "@" + value + ":" + v.getClass.getSimpleName
        // e.g. @List(...)
        case None => "@" + v.getClass.getSimpleName
      }
  }

  implicit class ConceptPrintOpts(v: Concept) {
    def printName: String =
      v.getNameString match {
        // e.g. $Person:Record(...)
        case Some(value) => "$" + value + ":" + v.getClass.getSimpleName
        // e.g. $List(...)
        case None => "$" + v.getClass.getSimpleName
      }
  }

  def tokenize(x: Any): Iterator[fansi.Str] = {
    val tree      = this.treeify(x)
    val renderer  = new Renderer(defaultWidth, colorApplyPrefix, colorLiteral, defaultIndent)
    val rendered  = renderer.rec(tree, 0, 0).iter
    val truncated = new Truncated(rendered, defaultWidth, defaultHeight)
    truncated
  }
}
