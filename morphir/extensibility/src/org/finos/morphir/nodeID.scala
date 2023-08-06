package org.finos.morphir

private[morphir] trait NodeIDExports { self: NameExports =>

  sealed case class NodePath(steps: Vector[NodePathStep]) { self =>
    import NodePathStep.*

    def /(step: NodePathStep): NodePath = NodePath(steps :+ step)
    def /(name: String): NodePath       = self / ChildByName(Name.fromString(name))

    @inline def isEmpty: Boolean = steps.isEmpty

    override def toString(): String =
      if (self.isEmpty) ""
      else {
        steps.map {
          case ChildByName(name)   => name.toCamelCase
          case ChildByIndex(index) => index.toString()
        }.mkString("#", ":", "")
      }
  }

  object NodePath {
    import NodePathStep.*
    val empty: NodePath = NodePath(Vector.empty)

    @inline def fromIterable(iterable: Iterable[NodePathStep]): NodePath = NodePath(iterable.toVector)

    def fromString(input: String): NodePath =
      if (input.isEmpty()) empty
      else {
        fromIterable(input.split(":").map { stepString =>
          stepString.toIntOption match {
            case Some(index) => NodePathStep.childByIndex(index)
            case None        => NodePathStep.childByName(stepString)
          }
        })
      }
  }

  sealed trait NodePathStep
  object NodePathStep {
    def childByName(input: String): NodePathStep = ChildByName(Name.fromString(input))
    def childByIndex(index: Int): NodePathStep   = ChildByIndex(index)

    final case class ChildByName(name: Name)  extends NodePathStep
    final case class ChildByIndex(index: Int) extends NodePathStep
  }
}
