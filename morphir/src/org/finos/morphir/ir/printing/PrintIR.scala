package org.finos.morphir.ir.printing

import fansi.Str
import pprint.{Renderer, Tree, Truncated}

import java.util.function.UnaryOperator
import org.finos.morphir.naming.*
import org.finos.morphir.ir.Value.{emptyTuple, Value as V}
import org.finos.morphir.ir.Value
import org.finos.morphir.ir.Type.Type as T
import org.finos.morphir.ir.Type
import pprint.Tree.{Apply, Literal}
//import org.finos.morphir.runtime.quick.Result //??
import zio.Chunk

object PrintIR {
  def apply(
      any: Any,
      detailLevel: DetailLevel = DetailLevel.BirdsEye,
      defaultWidth: Int = 150
  ) = new PrintIR(detailLevel, defaultWidth).apply(any)
}

case class DetailLevel(
    compressNames: Boolean,
    compressFQNames: Boolean,
    compressReferences: Boolean,
    showFieldNames: Boolean,
    fqnView: FQNameView,
    depthLimit: Option[Int]
)
object DetailLevel {
  object Detailed extends DetailLevel(
        compressNames = false,
        compressFQNames = false,
        compressReferences = false,
        showFieldNames = true,
        fqnView = FQNameView.Full,
        depthLimit = None
      )
  object Medium extends DetailLevel(
        compressNames = true,
        compressFQNames = true,
        compressReferences = false,
        showFieldNames = true,
        fqnView = FQNameView.ModuleLocal,
        depthLimit = Some(4)
      )
  object BirdsEye extends DetailLevel(
        compressNames = true,
        compressFQNames = true,
        compressReferences = true,
        showFieldNames = false,
        fqnView = FQNameView.LocalOnly,
        depthLimit = Some(2)
      )

}
sealed trait FQNameView {
  def apply(fqn: FQName): String
}
object FQNameView {

  object Full extends FQNameView {
    def apply(fqn: FQName): String = fqn.toString
  }
  object ModuleLocal extends FQNameView {
    def apply(fqn: FQName): String = s"${fqn.modulePath}:${fqn.localName.toCamelCase}"
  }

  object LocalOnly extends FQNameView {
    def apply(fqn: FQName): String = fqn.localName.toCamelCase
  }
}

sealed trait FieldNames

case class PrintIR(
    detailLevel: DetailLevel,
    val defaultWidth: Int
) extends pprint.Walker {
  val escapeUnicode                 = false
  val defaultHeight: Int            = Integer.MAX_VALUE
  val defaultIndent: Int            = 3
  val colorLiteral: fansi.Attrs     = fansi.Color.Green
  val colorApplyPrefix: fansi.Attrs = fansi.Color.Yellow
  var verbose: Boolean              = false

  override def additionalHandlers: PartialFunction[Any, Tree] = PartialFunction.empty

  def apply(x: Any, verbose: Boolean = false): fansi.Str = {
    this.verbose = verbose
    val tokenized = this.tokenize(x).toSeq
    fansi.Str.join(tokenized)
  }

  def withDepth(depth: Int): PrintIR = this.copy(detailLevel = detailLevel.copy(depthLimit = Some(depth)))

  // Can there be a "TreeifyWithDepth" option?
  def treeify(x: Any): Tree = this.treeify(x, escapeUnicode, this.detailLevel.showFieldNames)

  def fqnv(fqn: FQName): String = detailLevel.fqnView(fqn)
  def simplifyName(full: String): String =
    full
      .replace("$", ".")
      .replace("org.finos.morphir.ir.TypeModule.Type.", "")
      .replace("org.finos.morphir.ir.internal.Value.", "")
      .replace("org.finos.morphir.ir.internal.Pattern.", "")
      .replace("org.finos.morphir.ir.Literal.Literal.", "")

  /**
   * Extractor for any MorphirIR we want to treat specially for naming/depth limiting. Returns a prefix used to clarify
   * which nodes are which.
   */
  object AstNode {
    def unapply(any: Any): Option[String] = any match {
      case _: Type.Type[_]                                     => Some("T.")
      case _: Value.Value[_, _]                                => Some("V.")
      case _: Value.Pattern[_]                                 => Some("Pattern.")
      case _: Value.Specification[_]                           => Some("VSpec.")
      case _: Value.Definition[_, _]                           => Some("VDef.")
      case _: Type.Specification[_]                            => Some("TSpec.")
      case _: Type.Definition[_]                               => Some("TDef.")
      case other if other.getClass.getName.contains("RTValue") => Some("RT.")
      case _                                                   => None
    }
  }

  /**
   * Entry/dispatch site for treeify function Should: -check depth if appropriate -handle references? -handle FQNames?
   * -handle name shortening
   * @param x
   * @param escapeUnicode
   * @param showFieldNames
   * @return
   */

  override def treeify(x: Any, escapeUnicode: Boolean, showFieldNames: Boolean): Tree = {
    val node_prefix: String = x match {
      case AstNode(p) => p
      case _          => ""
    }
    val existing: Tree = detailLevel.depthLimit match {
      case Some(depth) => treeifyWithDepth(x, depth)
      case None        => treeifyNoDepth(x)
    }
    existing match {
      case Apply(prefix, body) => Apply(node_prefix + simplifyName(prefix), body)
      case Literal(body)       => Literal(node_prefix + simplifyName(body))
      case other               => other
    }

  }

  def treeifyWithDepth(x: Any, depth: Int): Tree =
    x match {
      case AstNode(_) if depth <= 0 =>
        x match {
          // Leafs can display even at depth 0
          // Assume compression for these cases
          case name: Name => Tree.Literal(name.toCamelCase)
          case T.Reference(_, fqn, Chunk()) =>
            Tree.Literal(s"Ref(${fqnv(fqn)})")
          case T.Reference(_, fqn, _) =>
            Tree.Literal(s"Ref(${fqnv(fqn)}) [..]")
          case V.Reference(_, fqn) =>
            Tree.Literal(s"Ref(${fqnv(fqn)})")
          case V.Literal(_, l) => treeifyHelper(l)
          case other           => Tree.Literal(s"${other.getClass.getName}(..)")
        }
      case AstNode(_) =>
        x match {
          case V.Record(_, fields) =>
            if (fields.length <= depth)
              withDepth(depth - fields.length).treeifyHelper(x)
            else
              Tree.Literal(x.getClass.getName)
          case other => withDepth(depth - 1).treeifyHelper(other)
        }
      case other => treeifyHelper(other)
    }
  def treeifyNoDepth(x: Any): Tree =
    treeifyHelper(x)

  def treeifyHelper(x: Any): Tree = x match {
    case name: Name if (detailLevel.compressNames) => Tree.Literal(name.toCamelCase)
    case fqn: FQName if (detailLevel.compressFQNames) =>
      Tree.Literal(fqnv(fqn))

    case T.Reference(_, fqn, Chunk()) if (detailLevel.compressReferences) =>
      Tree.Literal(s"Ref(${fqnv(fqn)})")

    case T.Reference(_, fqn, tpes) if (detailLevel.compressReferences) =>
      Tree.Apply(s"Ref(${fqnv(fqn)}):", tpes.map(treeify(_)).iterator)

    case other => super.treeify(other, escapeUnicode, this.detailLevel.showFieldNames)
  }

  def tokenize(x: Any): Iterator[fansi.Str] = {
    val tree      = this.treeify(x)
    val renderer  = new Renderer(defaultWidth, colorApplyPrefix, colorLiteral, defaultIndent)
    val rendered  = renderer.rec(tree, 0, 0).iter
    val truncated = new Truncated(rendered, defaultWidth, defaultHeight)
    truncated
  }
}
