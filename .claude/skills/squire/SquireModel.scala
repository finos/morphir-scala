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
    try
      val baseReal                        = base.toJava.toRealPath()
      val (existingAncestor, missingTail) = nearestExisting(candidate.toJava.toAbsolutePath.normalize)
      val candidateAncestorReal           = existingAncestor.toRealPath()
      if candidateAncestorReal.startsWith(baseReal) then
        val resolved = missingTail.foldLeft(candidateAncestorReal)(_.resolve(_))
        Result.Success(Path(resolved.toString))
      else pathEscapeFailure
    catch
      case error: java.io.IOException =>
        Result.Failure(SquireError.Failure("path", "could not resolve path under configured base", Present(error.getMessage)))

  private def nearestExisting(path: java.nio.file.Path): (java.nio.file.Path, List[java.nio.file.Path]) =
    @scala.annotation.tailrec
    def loop(current: java.nio.file.Path, missing: List[java.nio.file.Path]): (java.nio.file.Path, List[java.nio.file.Path]) =
      if java.nio.file.Files.exists(current) then (current, missing)
      else if java.nio.file.Files.exists(current, java.nio.file.LinkOption.NOFOLLOW_LINKS) then
        throw java.nio.file.NoSuchFileException(current.toString, null, "path contains a dangling symbolic link")
      else
        Option(current.getParent) match
          case Some(parent) => loop(parent, current.getFileName :: missing)
          case None         => throw java.nio.file.NoSuchFileException(current.toString)
    loop(path, Nil)

  private def pathEscapeFailure: Result[SquireError, Path] =
    Result.Failure(SquireError.Failure("path", "path escapes its configured base"))

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
