package org.finos.morphir.extensibility
import org.finos.morphir.*
import org.finos.morphir.internal.AllTypeLevelModules
import org.finos.morphir.naming.*

abstract class ExtensionsModule(val ir: AllTypeLevelModules) { self =>
  import ir._

  trait ExtensionNode extends HasId {}

  trait ExtensionMember extends RuntimeExtension {}

  trait RuntimeExtension extends ExtensionNode

  abstract class ExtensionModule extends RuntimeExtension {
    def exports: IndexedSeq[ExtensionMember]
  }

  abstract class ExtensionFunction extends ExtensionMember { self =>

    def specification: TypeSpecification[Any]

    def definition: TypeDefinition[Any]

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
