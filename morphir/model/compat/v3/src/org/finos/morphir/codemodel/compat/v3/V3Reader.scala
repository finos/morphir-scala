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
 * The rules match the v3-to-code-model lowering this replaces: v3 attributes are erased, except that a typed value's
 * type becomes its `inferredType`; constructors and bundle packages are ordered by name; a library's package version,
 * which v3 does not record, is left empty. Only format version 3 is read. Each `decode*` method reads one node kind
 * from its JSON text, which is also what a Morphir Compatibility Kit adapter needs.
 */
object V3Reader:
  type Read[A] = Result[V3ReadError, A]

  /** A whole `morphir-ir.json` document: one distribution per library, a `Bundle` expanded by package name. */
  def decodeIRFile(json: String): Read[Chunk[cm.Distribution]] = parsed(json)(V3Decoding.irFile(_, "$"))

  /** A bare distribution (`["Library", …]` or `["Bundle", …]`), without the `formatVersion` envelope. */
  def decodeDistribution(json: String): Read[Chunk[cm.Distribution]] = parsed(json)(V3Decoding.distribution(_, "$"))

  def decodeName(json: String): Read[Name]                          = parsed(json)(V3Decoding.name(_, "$"))
  def decodePath(json: String): Read[org.finos.morphir.naming.Path] = parsed(json)(V3Decoding.path(_, "$"))
  def decodeFQName(json: String): Read[FQName]                      = parsed(json)(V3Decoding.fqName(_, "$"))
  def decodeLiteral(json: String): Read[cm.Literal]                 = parsed(json)(V3Decoding.literal(_, "$"))
  def decodeType(json: String): Read[cm.Type]                       = parsed(json)(V3Decoding.tpe(_, "$"))
  def decodePattern(json: String): Read[cm.Pattern]                 = parsed(json)(V3Decoding.pattern(_, "$"))
  def decodeValue(json: String): Read[cm.Expr]                      = parsed(json)(V3Decoding.value(_, "$"))
  def decodeTypeDefinition(json: String): Read[cm.TypeDefinition]   = parsed(json)(V3Decoding.typeDefinition(_, "$"))
  def decodeTypeSpecification(json: String): Read[cm.TypeSpecification] =
    parsed(json)(V3Decoding.typeSpecification(_, "$"))
  def decodeValueDefinition(json: String): Read[cm.ValueDefinition] =
    parsed(json)(V3Decoding.valueDefinition(_, "$"))
  def decodeValueSpecification(json: String): Read[cm.ValueSpecification] =
    parsed(json)(V3Decoding.valueSpecification(_, "$"))
  def decodeModuleDefinition(json: String): Read[cm.ModuleDefinition] =
    parsed(json)(V3Decoding.moduleDefinition(_, "$"))
  def decodeModuleSpecification(json: String): Read[cm.ModuleSpecification] =
    parsed(json)(V3Decoding.moduleSpecification(_, "$"))

  private def parsed[A](json: String)(decode: Structure.Value => Read[A]): Read[A] =
    Json.decode[Structure.Value](json) match
      case Result.Success(value) => decode(value)
      case Result.Failure(error) => Result.fail(V3ReadError.MalformedJson(error.getMessage))
      case Result.Panic(error)   => Result.fail(V3ReadError.MalformedJson(error.getMessage))
end V3Reader
