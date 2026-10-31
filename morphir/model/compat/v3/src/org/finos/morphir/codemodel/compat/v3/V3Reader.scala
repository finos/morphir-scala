package org.finos.morphir.codemodel.compat.v3

import kyo.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.*

/**
 * Why a v3 document could not be read. Every case carries the JSON path of the fault (`$` is the document root) and the
 * Morphir Compatibility Kit diagnostic code a conforming reader reports for it.
 */
enum V3ReadError derives CanEqual:
  /** The input is not well-formed JSON. */
  case MalformedJson(details: String)

  /** The document declares a `formatVersion` other than 3. */
  case UnsupportedFormatVersion(version: String)

  /** A tagged array names a tag this node kind does not have. */
  case UnknownTag(path: String, tag: String)

  /** A node does not have the shape its kind requires. */
  case WrongShape(path: String, expected: String)

  /** Where in the input the fault is, as a JSON path from the document root. */
  def location: String = this match
    case MalformedJson(_)            => "$"
    case UnsupportedFormatVersion(_) => "$.formatVersion"
    case UnknownTag(path, _)         => path
    case WrongShape(path, _)         => path

  /** The MCK diagnostic code for this fault. */
  def code: String = this match
    case MalformedJson(_)            => V3ReadError.Codes.InvalidJson
    case UnsupportedFormatVersion(_) => V3ReadError.Codes.UnsupportedFormatVersion
    case UnknownTag(_, _)            => V3ReadError.Codes.UnknownTag
    case WrongShape(_, _)            => V3ReadError.Codes.InvalidShape

  /** A one-line description of the fault, starting with its location where it has one. */
  def message: String = this match
    case MalformedJson(details)            => s"not well-formed JSON: $details"
    case UnsupportedFormatVersion(version) =>
      s"formatVersion $version is not supported; only 3 is. Regenerate the IR with current morphir-elm."
    case UnknownTag(path, tag)      => s"$path: unknown tag '$tag'"
    case WrongShape(path, expected) => s"$path: expected $expected"
end V3ReadError

object V3ReadError:
  /** The MCK diagnostic codes the v3 reader reports. */
  object Codes:
    val InvalidJson              = "invalid_json"
    val UnsupportedFormatVersion = "unsupported_format_version"
    val UnknownTag               = "unknown_tag"
    val InvalidShape             = "invalid_shape"

/**
 * Reads Morphir IR written in format version 3 - the `morphir-ir.json` that morphir-elm produces - straight into the
 * code model, without the classic `org.finos.morphir.ir` library.
 *
 * The result is the one the classic v3 decoder and `V3Lowering` together produce:
 *
 *   - Attributes are erased, except that a typed value's or pattern's type becomes its `inferredType`. An attribute
 *     counts as a type when it is a non-empty array that starts with a tag string; anything else, such as the `{}`
 *     morphir-elm writes for an untyped value, gives empty attributes.
 *   - Constructors, `LetRecursion` definitions and `UpdateRecord` fields are ordered by name, word by word; bundle
 *     libraries are ordered by package name. v3 holds each of them in a map, so a repeated name keeps its last entry.
 *   - A documentation string is split into lines; an empty one is no documentation.
 *   - A library's package version, which v3 does not record, is left empty.
 *   - Object members a node does not use, such as the module `doc` morphir-elm writes, are ignored.
 *
 * Every method returns a [[V3ReadError]] rather than throwing: `MalformedJson` when the text is not JSON, `UnknownTag`
 * when a tagged array names a tag its node kind lacks, and `WrongShape` for any other structural fault, each with the
 * JSON path of the fault. Only format version 3 is read; [[decodeIRFile]] rejects any other with
 * `UnsupportedFormatVersion`. Each `decode*` method reads one node kind from its JSON text, which is also what a
 * Morphir Compatibility Kit adapter needs.
 */
object V3Reader:

  /** The outcome of reading: the decoded value, or why it could not be read. */
  type Read[A] = Result[V3ReadError, A]

  /**
   * A whole `morphir-ir.json` document, `{"formatVersion": 3, "distribution": …}`: one code-model library for a
   * `Library`, one per package for a `Bundle`, ordered by package name.
   */
  def decodeIRFile(json: String): Read[Chunk[cm.Distribution]] = parsed(json)(V3Decoding.irFile(_, "$"))

  /** A bare distribution (`["Library", …]` or `["Bundle", …]`), without the `formatVersion` envelope. */
  def decodeDistribution(json: String): Read[Chunk[cm.Distribution]] = parsed(json)(V3Decoding.distribution(_, "$"))

  /** A name: an array of words, such as `["value","in","u","s","d"]`. */
  def decodeName(json: String): Read[Name] = parsed(json)(V3Decoding.name(_, "$"))

  /** A path: an array of names, such as `[["morphir"],["s","d","k"]]`. */
  def decodePath(json: String): Read[org.finos.morphir.naming.Path] = parsed(json)(V3Decoding.path(_, "$"))

  /** A fully qualified name: `[packagePath, modulePath, localName]`. */
  def decodeFQName(json: String): Read[FQName] = parsed(json)(V3Decoding.fqName(_, "$"))

  /**
   * A literal, such as `["WholeNumberLiteral",42]`. A `WholeNumberLiteral` must fit in 64 bits and a `CharLiteral` must
   * hold one code point; a `DecimalLiteral` is read from its text.
   */
  def decodeLiteral(json: String): Read[cm.Literal] = parsed(json)(V3Decoding.literal(_, "$"))

  /** A type, such as `["Reference",{},fqName,[typeArguments…]]`; its attributes are erased. */
  def decodeType(json: String): Read[cm.Type] = parsed(json)(V3Decoding.tpe(_, "$"))

  /** A pattern, such as `["AsPattern",attributes,pattern,name]`. */
  def decodePattern(json: String): Read[cm.Pattern] = parsed(json)(V3Decoding.pattern(_, "$"))

  /** A value, such as `["Apply",attributes,function,argument]`, as a code-model expression. */
  def decodeValue(json: String): Read[cm.Expr] = parsed(json)(V3Decoding.value(_, "$"))

  /** A type definition: `TypeAliasDefinition` or `CustomTypeDefinition`, whose constructors are ordered by name. */
  def decodeTypeDefinition(json: String): Read[cm.TypeDefinition] = parsed(json)(V3Decoding.typeDefinition(_, "$"))

  /** A type specification: `TypeAliasSpecification`, `OpaqueTypeSpecification` or `CustomTypeSpecification`. */
  def decodeTypeSpecification(json: String): Read[cm.TypeSpecification] =
    parsed(json)(V3Decoding.typeSpecification(_, "$"))

  /**
   * A value definition, `{"inputTypes": …, "outputType": …, "body": …}`. v3 records a value's access on its module
   * entry, not on the definition, so a definition read on its own is public; [[decodeModuleDefinition]] gives each
   * definition the access of its entry.
   */
  def decodeValueDefinition(json: String): Read[cm.ValueDefinition] =
    parsed(json)(V3Decoding.valueDefinition(_, "$"))

  /** A value specification, `{"inputs": …, "output": …}`. */
  def decodeValueSpecification(json: String): Read[cm.ValueSpecification] =
    parsed(json)(V3Decoding.valueSpecification(_, "$"))

  /** A module definition, `{"types": …, "values": …}`, each entry access-controlled and documented. */
  def decodeModuleDefinition(json: String): Read[cm.ModuleDefinition] =
    parsed(json)(V3Decoding.moduleDefinition(_, "$"))

  /** A module specification, `{"types": …, "values": …}`, each entry documented. */
  def decodeModuleSpecification(json: String): Read[cm.ModuleSpecification] =
    parsed(json)(V3Decoding.moduleSpecification(_, "$"))

  private def parsed[A](json: String)(decode: Structure.Value => Read[A]): Read[A] =
    Json.decode[Structure.Value](json) match
      case Result.Success(value) => decode(value)
      case Result.Failure(error) => Result.fail(V3ReadError.MalformedJson(error.getMessage))
      case Result.Panic(error)   => Result.fail(V3ReadError.MalformedJson(error.getMessage))
end V3Reader
