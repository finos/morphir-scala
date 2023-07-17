package org.finos.morphir.universe.ir

import zio.prelude.*

/**
 * Represents a path in the IR.
 * ==Overview==
 * A NodeID can have two slightly different structures depending on if we are refering to modules or definitions
 * (types/values).
 *
 *   - When refefering to modules: `"module:<Package>:<Module>"`
 *   - When refering to definitions: `"type\value:<Package>:<Module><localName>#<nodePath>"`, where nodePath is optional
 *
 * Examples of valid NodeIDs:
 *   - "module:Morphir.Reference.Model:BooksAndRecords"
 *   - "type:Morphir.Reference.Model:BooksAndRecords:deal"
 *   - "value:Morphir.Reference.Model:BooksAndRecords:deal#1"
 *
 * ==Referring to modules==
 * We can refer to modules by their Qualified Name, with the module: prefix
 *
 * For example: `"module:Morphir.Reference.Model:BooksAndRecords"` refers to the `Books and Records` module inside the
 * `Morphir.Reference.Model` package.
 */
sealed trait NodeId
object NodeId {

  def fromString(input: String): Either[Error, NodeId] = {
    def mapToTypeOrValue(packageName: String, moduleName: String, defNameWithSuffix: String, nodePath: String) = {
      def defName(suffix: String) = defNameWithSuffix.dropRight(suffix.length())
      if (defNameWithSuffix.endsWith(".value")) {
        Right(ValueId(FQName.fqn(packageName, moduleName, defName(".value")), NodePath.fromString(nodePath)))
      } else {
        Right(ValueId(FQName.fqn(packageName, moduleName, defName(".type")), NodePath.fromString(nodePath)))
      }
    }

    input.split(":") match {
      case Array(packageName, moduleName) =>
        Right(ModuleId(Path(packageName), Path(moduleName)))
      case Array(packageName, moduleName, localName) =>
        if (localName.contains("#")) {
          localName.split("#") match {
            case Array(defName, path) => mapToTypeOrValue(packageName, moduleName, defName, path)
            case _                    => Left(Error.InvalidNodeId(input))
          }
        } else {
          mapToTypeOrValue(packageName, moduleName, localName, "")
        }
      case _ =>
        Left(Error.InvalidNodeId(input))
    }
  }

  final case class TypeId(name: FQName, path: NodePath)          extends NodeId
  final case class ValueId(name: FQName, path: NodePath)         extends NodeId
  final case class ModuleId(packagePath: Path, modulePath: Path) extends NodeId

  sealed abstract class Error(errorMessage: String) extends Exception(errorMessage)
  object Error {
    final case class InvalidPath(input: String, errorMessage: String) extends Error(errorMessage)
    final case class InvalidNodeId(input: String, errorMessage: String) extends Error(errorMessage) {
      def this(input: String) = this(input, s"Invalid NodeId: $input")
    }

    object InvalidNodeId {
      def apply(input: String): InvalidNodeId = new InvalidNodeId(input)
    }
  }
}
