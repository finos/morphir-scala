//| scalaVersion: 3.8.4
//| mvnDeps:
//| - io.getkyo::kyo-core:1.0.0-RC6
//| - io.getkyo::kyo-schema-json:1.0.0-RC6
//| - io.getkyo::kyo-schema-yaml:1.0.0-RC6

import kyo.*

sealed abstract class SquireError(message: String) extends RuntimeException(message)

object SquireError:
  final case class Failure(area: String, message: String, detail: Maybe[String] = Absent)
      extends SquireError(message)

object SquireJson:
  def encode[A: Schema](value: A): String = Json.encode(value)

  def decode[A: Schema](text: String): Result[DecodeException, A] = Json.decode(text)

  def pretty(value: Structure.Value): String =
    render(value, 0) + "\n"

  private def render(value: Structure.Value, depth: Int): String =
    value match
      case Structure.Value.Record(fields) => renderObject(fields, depth)
      case Structure.Value.VariantCase(name, value) =>
        renderObject(Chunk("name" -> Structure.Value.Str(name), "value" -> value), depth)
      case Structure.Value.Sequence(elements) => renderArray(elements, depth)
      case Structure.Value.MapEntries(entries) =>
        renderArray(entries.map { case (key, value) => Structure.Value.Sequence(Chunk(key, value)) }, depth)
      case Structure.Value.Str(value)     => Json.encode(value)
      case Structure.Value.Bool(value)    => Json.encode(value)
      case Structure.Value.Integer(value) => Json.encode(value)
      case Structure.Value.Decimal(value) => Json.encode(value)
      case Structure.Value.BigNum(value)  => Json.encode(value)
      case Structure.Value.Null           => "null"
      case Structure.Value.Bytes(_) | Structure.Value.Instant(_) | Structure.Value.Duration(_) =>
        throw SquireError.Failure("json", "value cannot be represented as deterministic JSON")

  private def renderObject(fields: Chunk[(String, Structure.Value)], depth: Int): String =
    if fields.isEmpty then "{}"
    else
      fields
        .map { case (name, value) =>
          s"${indent(depth + 1)}${Json.encode(name)}: ${render(value, depth + 1)}"
        }
        .mkString("{\n", ",\n", s"\n${indent(depth)}}")

  private def renderArray(elements: Chunk[Structure.Value], depth: Int): String =
    if elements.isEmpty then "[]"
    else
      elements
        .map(value => s"${indent(depth + 1)}${render(value, depth + 1)}")
        .mkString("[\n", ",\n", s"\n${indent(depth)}]")

  private def indent(depth: Int): String = "  " * depth

object SquireYaml:
  def encode[A: Schema](value: A): String = Yaml.encode(value)

  def decode[A: Schema](text: String): Result[DecodeException, A] = Yaml.decode(text)

object SquirePaths:
  def render(path: Path): String = path.toString

  def isUnder(child: Path, base: Path): Boolean =
    child.parts.take(base.parts.size) == base.parts

  def resolveUnder(candidate: Path, base: Path): Result[SquireError, Path] =
    if isUnder(candidate, base) then Result.Success(candidate)
    else Result.Failure(SquireError.Failure("path", "path escapes its configured base"))

  def findRepoRoot(from: Path): Maybe[Path] < Sync =
    def loop(path: Path): Maybe[Path] < Sync =
      (path / ".git").exists.flatMap { exists =>
        if exists then Present(path)
        else
          path.parent match
            case Present(parent) => loop(parent)
            case Absent          => Absent
      }
    loop(from)
