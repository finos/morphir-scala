package org.finos.morphir.extensibility
import org.finos.morphir._
import org.finos.morphir.naming._

trait ExtensionsModule { self: TypeModule with TypeSpecModule =>
  trait ExtensionNode extends HasId {}

  trait ExtensionMember extends ExtensionNode {}

  abstract class ExtensionModule extends ExtensionNode {
    def exports: IndexedSeq[ExtensionMember]
  }

  abstract class ExtensionFunction extends ExtensionMember { self =>

    def specification: TypeSpecification[Any]

    def definition: TypeDefinition

    def name: FQName
    def nodeID: NodeID = NodeID.ValueID(name, NodePath.empty)
    def invoke(args: List[Any]): Any

    def arity: Int

    def defaultHints: Hints = Hints.empty

    def invoke(args: List[Any], hints: Hints): Any
  }

  object ExtensionFunction {
//    def unapply(value:Any):Option[()]
  }

}
