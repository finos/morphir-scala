package org.finos.morphir.ir.printing

import fansi.Str
import pprint.{Renderer, Tree, Truncated}
import java.util.function.UnaryOperator
import org.finos.morphir.naming._
import org.finos.morphir.ir.Value.{Value => V}
import org.finos.morphir.ir.Value
import org.finos.morphir.ir.Type.{Type => T}
import org.finos.morphir.ir.Type
import pprint.Tree.Literal
import zio.Chunk

object PrintIR {
  def apply(
      any: Any,
      detailLevel: DetailLevel = DetailLevel.BirdsEye,
      fieldNames: FieldNames = FieldNames.Hide,
      defaultWidth: Int = 150
  ) = new PrintIR(detailLevel, fieldNames, defaultWidth).apply(any)
}

sealed trait DetailLevel {
  def compressFQNames: Boolean
  def compressReferences: Boolean
}
object DetailLevel {
  object Detailed extends DetailLevel {
    def compressFQNames    = false
    def compressReferences = false
  }
  object Medium extends DetailLevel {
    def compressFQNames    = true
    def compressReferences = false
  }
  object BirdsEye extends DetailLevel {
    def compressFQNames    = true
    def compressReferences = true
  }
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

  //Can there be a "TreeifyWithDepth" option?
  def treeify(x: Any): Tree = this.treeify(x, escapeUnicode, showFieldNames)

  object MorphirValue {
    def unapply(any: Any) =
      any match {
        case _: Value.Value[_, _] =>
          Some(
            any
              .getClass()
              .getName()
              .replace("org.finos.morphir.ir.internal.", "")
              .replace("$", ".")
          )
        case _ =>
          None
      }
  }

  override def treeify(x: Any, escapeUnicode: Boolean, showFieldNames: Boolean): Tree = x match {
    case fqname: FQName if (detailLevel.compressFQNames) =>
      Tree.Literal(fqname.toString)

    case T.Reference(_, fqname, Chunk()) if (detailLevel.compressReferences) =>
      Tree.Literal(s"${fqname.toString}")

    case T.Reference(_, fqname, tpes) if (detailLevel.compressReferences) =>
      Tree.Literal(s"${fqname.toString}[${tpes.map(_.toString).mkString(", ")}]")

    case MorphirValue(name) =>
      val existingTree =
        super.treeify(x, escapeUnicode, showFieldNames) match {
          case Tree.Apply(_, content) => content
          case other                  => Iterator(other)
        }
      Tree.Apply(s"${name}", existingTree)

    case other => super.treeify(other, escapeUnicode, showFieldNames)
  }

  def tokenize(x: Any): Iterator[fansi.Str] = {
    val tree      = this.treeify(x)
    val renderer  = new Renderer(defaultWidth, colorApplyPrefix, colorLiteral, defaultIndent)
    val rendered  = renderer.rec(tree, 0, 0).iter
    val truncated = new Truncated(rendered, defaultWidth, defaultHeight)
    truncated
  }
}
