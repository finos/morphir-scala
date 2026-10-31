package org.finos.morphir.codemodel.compat.v3

import kyo.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.*

/** The node decoders behind [[V3Reader]]. Each takes the parsed JSON node and its JSON path, for error locations. */
private[v3] object V3Decoding:
  import V3Reader.Read

  private type Value = Structure.Value

  def irFile(value: Value, path: String): Read[Chunk[cm.Distribution]] =
    value match
      case Structure.Value.Record(fields) =>
        val byName = fields.toMap
        byName.get("formatVersion") match
          case None => Result.fail(V3ReadError.WrongShape(s"$path.formatVersion", "a formatVersion member"))
          case Some(Structure.Value.Integer(3)) =>
            byName.get("distribution") match
              case Some(distributionValue) => distribution(distributionValue, s"$path.distribution")
              case None => Result.fail(V3ReadError.WrongShape(s"$path.distribution", "a distribution member"))
          case Some(other) => Result.fail(V3ReadError.UnsupportedFormatVersion(render(other)))
      case _ => Result.fail(V3ReadError.WrongShape(path, "an object with formatVersion and distribution"))

  def distribution(value: Value, path: String): Read[Chunk[cm.Distribution]] =
    tagged(value, path) { (tag, _) =>
      tag match
        case "Library" | "Bundle" => notYet(path, s"$tag distribution")
        case other                => Result.fail(V3ReadError.UnknownTag(path, other))
    }

  def name(value: Value, path: String): Read[Name]                              = notYet(path, "Name")
  def path(value: Value, path: String): Read[org.finos.morphir.naming.Path]     = notYet(path, "Path")
  def fqName(value: Value, path: String): Read[FQName]                          = notYet(path, "FQName")
  def literal(value: Value, path: String): Read[cm.Literal]                     = notYet(path, "Literal")
  def tpe(value: Value, path: String): Read[cm.Type]                            = notYet(path, "Type")
  def pattern(value: Value, path: String): Read[cm.Pattern]                     = notYet(path, "Pattern")
  def value(value: Value, path: String): Read[cm.Expr]                          = notYet(path, "Value")
  def typeDefinition(value: Value, path: String): Read[cm.TypeDefinition]       = notYet(path, "TypeDefinition")
  def typeSpecification(value: Value, path: String): Read[cm.TypeSpecification] =
    notYet(path, "TypeSpecification")
  def valueDefinition(value: Value, path: String): Read[cm.ValueDefinition]       = notYet(path, "ValueDefinition")
  def valueSpecification(value: Value, path: String): Read[cm.ValueSpecification] =
    notYet(path, "ValueSpecification")
  def moduleDefinition(value: Value, path: String): Read[cm.ModuleDefinition]       = notYet(path, "ModuleDefinition")
  def moduleSpecification(value: Value, path: String): Read[cm.ModuleSpecification] =
    notYet(path, "ModuleSpecification")

  /** A v3 tagged array: `["Tag", arguments…]`. */
  private def tagged[A](value: Value, path: String)(f: (String, Chunk[Value]) => Read[A]): Read[A] =
    value match
      case Structure.Value.Sequence(items) if items.nonEmpty =>
        items.head match
          case Structure.Value.Str(tag) => f(tag, items.drop(1))
          case _                        => Result.fail(V3ReadError.WrongShape(s"$path[0]", "a tag string"))
      case _ => Result.fail(V3ReadError.WrongShape(path, "a tagged array"))

  private def render(value: Value): String = value match
    case Structure.Value.Integer(n) => n.toString
    case Structure.Value.Str(s)     => s"\"$s\""
    case other                      => other.toString

  // Placeholder until the node decoders land; reports a typed error rather than guessing.
  private def notYet[A](path: String, node: String): Read[A] =
    Result.fail(V3ReadError.WrongShape(path, s"$node (decoding not implemented yet)"))
end V3Decoding
