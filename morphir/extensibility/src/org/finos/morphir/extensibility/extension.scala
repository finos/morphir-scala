package org.finos.morphir.extensibility
import org.finos.morphir.naming._
import org.finos.morphir._

trait ExtensionNode   extends HasId         {}
trait ExtensionMember extends ExtensionNode {}
abstract class ExtensionModule extends ExtensionNode {
  def exports: IndexedSeq[ExtensionMember]
}

abstract class ExtensionFunction extends ExtensionMember { self =>
  def spec: TypeModule#TypeSpecification
  def defn: TypeModule#TypeDefinition
  def name: FQName = self.id match {
    case NodeID.ValueID(fqn, _) => fqn
    case _                      => throw new Exception("ExtensionFunction.id must be a ValueID")
  }
  def invoke(args: List[Any]): Any
  def arity: Int
  def defaultHints: Hints = Hints.empty
  def invoke(args: List[Any], hints: Hints): Any
}

object ExtensionFunction {
  def unapply(input: Any): Option[(NodeID, FQName, Int, Hints)] = input match {
    case extFunc: ExtensionFunction => Some((extFunc.id, extFunc.name, extFunc.arity, extFunc.defaultHints))
    case _                          => None
  }
}

sealed trait TypeSpec
sealed trait TypeDef
