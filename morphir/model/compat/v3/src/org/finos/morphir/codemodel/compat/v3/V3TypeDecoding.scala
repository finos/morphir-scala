package org.finos.morphir.codemodel.compat.v3

import kyo.Chunk
import org.finos.morphir.codemodel as cm

import scala.math.Ordering.Implicits.seqOrdering

/**
 * Decoders for v3 types, type definitions, type specifications and constructors.
 *
 * A type's attribute (argument zero of each tagged node) is erased: the code model's `TypeAttributes` has no slot for
 * v3's arbitrary type attribute, so any JSON is accepted there and the decoded type carries `TypeAttributes.empty`.
 */
private[v3] object V3TypeDecoding:
  import V3JsonShape.*
  import V3NamingDecoding.*
  import V3Reader.Read

  def tpe(value: Value, path: String): Read[cm.Type] =
    tagged(value, path) { (tag, args) =>
      val attributes              = cm.TypeAttributes.empty
      def arg(index: Int): String = at(path, index + 1)
      tag match
        case "Variable" =>
          arguments(tag, args, 2, path)(name(args(1), arg(1)).map(cm.Type.Variable(attributes, _)))
        case "Reference" =>
          arguments(tag, args, 3, path) {
            for
              reference <- fqName(args(1), arg(1))
              typeArgs  <- list(args(2), arg(2))(tpe)
            yield cm.Type.Reference(attributes, reference, typeArgs)
          }
        case "Tuple" =>
          arguments(tag, args, 2, path)(list(args(1), arg(1))(tpe).map(cm.Type.Tuple(attributes, _)))
        case "Record" =>
          arguments(tag, args, 2, path)(list(args(1), arg(1))(field).map(cm.Type.Record(attributes, _)))
        case "ExtensibleRecord" =>
          arguments(tag, args, 3, path) {
            for
              variable <- name(args(1), arg(1))
              fields   <- list(args(2), arg(2))(field)
            yield cm.Type.ExtensibleRecord(attributes, variable, fields)
          }
        case "Function" =>
          arguments(tag, args, 3, path) {
            for
              argumentType <- tpe(args(1), arg(1))
              returnType   <- tpe(args(2), arg(2))
            yield cm.Type.Function(attributes, argumentType, returnType)
          }
        case "Unit" => arguments(tag, args, 1, path)(succeed(cm.Type.Unit(attributes)))
        case other  => unknownTag(path, other)
    }

  /** A record type's field: `{"name": …, "tpe": …}`. */
  private def field(value: Value, path: String): Read[cm.Field] =
    obj(value, path, "an object with name and tpe") { members =>
      members.read("name")(name).flatMap(fieldName => members.read("tpe")(tpe).map(cm.Field(fieldName, _)))
    }

  def typeDefinition(value: Value, path: String): Read[cm.TypeDefinition] =
    tagged(value, path) { (tag, args) =>
      def arg(index: Int): String = at(path, index + 1)
      tag match
        case "TypeAliasDefinition" =>
          arguments(tag, args, 2, path) {
            for
              params <- typeParameters(args(0), arg(0))
              body   <- tpe(args(1), arg(1))
            yield cm.TypeDefinition.TypeAliasDefinition(params, body)
          }
        case "CustomTypeDefinition" =>
          arguments(tag, args, 2, path) {
            for
              params <- typeParameters(args(0), arg(0))
              ctors  <- accessControlled(args(1), arg(1))(constructors)
            yield cm.TypeDefinition.CustomTypeDefinition(params, ctors)
          }
        case other => unknownTag(path, other)
    }

  def typeSpecification(value: Value, path: String): Read[cm.TypeSpecification] =
    tagged(value, path) { (tag, args) =>
      def arg(index: Int): String = at(path, index + 1)
      tag match
        case "TypeAliasSpecification" =>
          arguments(tag, args, 2, path) {
            for
              params <- typeParameters(args(0), arg(0))
              body   <- tpe(args(1), arg(1))
            yield cm.TypeSpecification.TypeAliasSpecification(params, body)
          }
        case "OpaqueTypeSpecification" =>
          arguments(tag, args, 1, path)(
            typeParameters(args(0), arg(0)).map(cm.TypeSpecification.OpaqueTypeSpecification(_))
          )
        case "CustomTypeSpecification" =>
          arguments(tag, args, 2, path) {
            for
              params <- typeParameters(args(0), arg(0))
              ctors  <- constructors(args(1), arg(1))
            yield cm.TypeSpecification.CustomTypeSpecification(params, ctors)
          }
        case other => unknownTag(path, other)
    }

  /**
   * A custom type's constructors: `[[name, [[argName, type], …]], …]`. v3 holds them in a map keyed by name, so the
   * input order carries no meaning (and a repeated name keeps its last entry); they are ordered by name - compared word
   * by word, as the v3 lowering does - so that equal documents decode to equal constructor sequences.
   */
  def constructors(value: Value, path: String): Read[Chunk[cm.Constructor]] =
    list(value, path)((entry, entryPath) => pair(entry, entryPath)(name, constructorArguments)).map { entries =>
      Chunk.from(entries.toMap.toSeq.sortBy(_._1.toList).map((ctorName, args) => cm.Constructor(ctorName, args)))
    }

  private def constructorArguments(value: Value, path: String): Read[Chunk[cm.Parameter]] =
    list(value, path)(parameter)

  /** A `[name, type]` pair, as in constructor arguments and value specification inputs. */
  def parameter(value: Value, path: String): Read[cm.Parameter] =
    pair(value, path)(name, tpe).map((parameterName, parameterType) => cm.Parameter(parameterName, parameterType))

  private def typeParameters(value: Value, path: String): Read[Chunk[org.finos.morphir.naming.Name]] =
    list(value, path)(name)
end V3TypeDecoding
