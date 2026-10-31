package org.finos.morphir.codemodel.compat.v3

import kyo.Chunk
import kyo.Result
import kyo.Structure
import org.finos.morphir.codemodel as cm

import scala.annotation.tailrec

/**
 * The JSON shapes v3 is built from - tagged arrays, fixed-length arrays, objects with named members, lists - read from
 * a parsed `Structure.Value` with the JSON path of each part, so that a fault is reported where it occurs.
 *
 * Paths use JSONPath notation from the document root: `$.distribution[3].modules[0][1]`.
 */
private[v3] object V3JsonShape:
  import V3Reader.Read

  type Value = Structure.Value

  def succeed[A](value: A): Read[A]                          = Result.succeed(value)
  def wrongShape[A](path: String, expected: String): Read[A] = Result.fail(V3ReadError.WrongShape(path, expected))
  def unknownTag[A](path: String, tag: String): Read[A]      = Result.fail(V3ReadError.UnknownTag(path, tag))

  /** The path of element `index` of the array at `path`. */
  def at(path: String, index: Int): String = s"$path[$index]"

  /** The path of member `name` of the object at `path`. */
  def member(path: String, name: String): String = s"$path.$name"

  /**
   * A v3 tagged array, `["Tag", arguments…]`. `f` receives the tag and the arguments; argument `i` (from zero) sits at
   * `at(path, i + 1)`, since the tag is element zero.
   */
  def tagged[A](value: Value, path: String)(f: (String, Chunk[Value]) => Read[A]): Read[A] =
    value match
      case Structure.Value.Sequence(items) if items.nonEmpty =>
        items.head match
          case Structure.Value.Str(tag) => f(tag, items.drop(1))
          case _                        => wrongShape(at(path, 0), "a tag string")
      case _ => wrongShape(path, "a tagged array")

  /** Requires a tagged node to carry exactly `arity` arguments after its tag. */
  def arguments[A](tag: String, args: Chunk[Value], arity: Int, path: String)(f: => Read[A]): Read[A] =
    if args.size == arity then f
    else wrongShape(path, s"$tag with ${arity} argument${if arity == 1 then "" else "s"} after the tag")

  /** An array of any length. */
  def array(value: Value, path: String): Read[Chunk[Value]] =
    value match
      case Structure.Value.Sequence(items) => succeed(items)
      case _                               => wrongShape(path, "an array")

  /** An array of exactly `size` elements. */
  def tuple(value: Value, path: String, size: Int): Read[Chunk[Value]] =
    value match
      case Structure.Value.Sequence(items) if items.size == size => succeed(items)
      case _                                                     => wrongShape(path, s"an array of $size elements")

  /** An array whose every element `decode` reads; element `i` is decoded at `at(path, i)`. */
  def list[A](value: Value, path: String)(decode: (Value, String) => Read[A]): Read[Chunk[A]] =
    array(value, path).flatMap(items => traverse(items, path)(decode))

  /** A two-element array, such as a `[name, value]` entry. */
  def pair[A, B](value: Value, path: String)(
      first: (Value, String) => Read[A],
      second: (Value, String) => Read[B]
  ): Read[(A, B)] =
    tuple(value, path, 2).flatMap { items =>
      first(items(0), at(path, 0)).flatMap(a => second(items(1), at(path, 1)).map(b => (a, b)))
    }

  /** A JSON object; `f` reads its members. Members the decoder does not ask for are ignored. */
  def obj[A](value: Value, path: String, expected: String)(f: Members => Read[A]): Read[A] =
    value match
      case Structure.Value.Record(fields) => f(Members(fields, path))
      case _                              => wrongShape(path, expected)

  /** The members of a JSON object at `path`. */
  final case class Members(fields: Chunk[(String, Value)], path: String):
    /** The member `name`, or a fault naming it when it is absent. */
    def required(name: String): Read[Value] =
      fields.find(_._1 == name) match
        case Some((_, value)) => succeed(value)
        case None             => wrongShape(member(path, name), s"a $name member")

    /** Reads the member `name` with `decode`, at its own path. */
    def read[A](name: String)(decode: (Value, String) => Read[A]): Read[A] =
      required(name).flatMap(value => decode(value, member(path, name)))

    /** Reads the member `name` with `decode` when it is present and not `null`. */
    def readOptional[A](name: String)(decode: (Value, String) => Read[A]): Read[Option[A]] =
      fields.find(_._1 == name) match
        case None | Some((_, Structure.Value.Null)) => succeed(None)
        case Some((_, value))                       => decode(value, member(path, name)).map(Some(_))

  def string(value: Value, path: String): Read[String] =
    value match
      case Structure.Value.Str(text) => succeed(text)
      case _                         => wrongShape(path, "a string")

  /** v3's `Access`: `"Public"` or `"Private"`. */
  def access(value: Value, path: String): Read[cm.Access] =
    value match
      case Structure.Value.Str("Public")  => succeed(cm.Access.Public)
      case Structure.Value.Str("Private") => succeed(cm.Access.Private)
      case _                              => wrongShape(path, "\"Public\" or \"Private\"")

  /** v3's `AccessControlled`: `{"access": …, "value": …}`. */
  def accessControlled[A](value: Value, path: String)(
      decode: (Value, String) => Read[A]
  ): Read[cm.AccessControlled[A]] =
    obj(value, path, "an object with access and value") { members =>
      members.read("access")(access).flatMap(level => members.read("value")(decode).map(cm.AccessControlled(level, _)))
    }

  /**
   * v3's `Documented`: `{"doc": "…", "value": …}`. An empty doc string decodes to no documentation; any other is split
   * into lines.
   */
  def documented[A](value: Value, path: String)(decode: (Value, String) => Read[A]): Read[cm.Documented[A]] =
    obj(value, path, "an object with doc and value") { members =>
      members.read("doc")(string).flatMap { doc =>
        members.read("value")(decode).map(cm.Documented(documentation(doc), _))
      }
    }

  def documentation(doc: String): Option[cm.Documentation] =
    if doc.isEmpty then None else Some(cm.Documentation(Chunk.from(doc.linesIterator.toSeq)))

  /** Decodes each item, stopping at the first fault; item `i` is decoded at `at(path, i)`. */
  def traverse[A](items: Chunk[Value], path: String)(decode: (Value, String) => Read[A]): Read[Chunk[A]] =
    @tailrec
    def loop(index: Int, reversed: List[A]): Read[Chunk[A]] =
      if index == items.size then succeed(Chunk.from(reversed.reverse))
      else
        decode(items(index), at(path, index)) match
          case Result.Success(decoded) => loop(index + 1, decoded :: reversed)
          case Result.Failure(error)   => Result.fail(error)
          case Result.Panic(cause)     => Result.panic(cause)
    loop(0, Nil)
end V3JsonShape
