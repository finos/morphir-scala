package org.finos.morphir.codemodel.compat.v3

import kyo.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.*

/**
 * Reads Morphir IR written in format version 3 - the `morphir-ir.json` that morphir-elm produces - straight into the
 * code model, without the classic ZIO-based IR library.
 *
 * A document's `formatVersion` is read first, by finos/morphir `docs/spec/ir/format-version.md`. The reader supports
 * [[SupportTable]], `[3.0.0,3.2.0)`: the integer `3` and the release strings from `"3.0.0"` up to, not including,
 * `"3.2.0"`. 3.1.0 added the `Specs` distribution, which a document must declare 3.1.0 or later to hold.
 *
 * The result is the one the classic v3 decoder and `V3Lowering` together produce, with the module documentation,
 * `Specs` distributions and derived type specifications that those never read:
 *
 *   - Attributes are erased, except that a typed value's or pattern's type becomes its `inferredType`. An attribute
 *     counts as a type when it is a non-empty array that starts with a tag string; anything else, such as the `{}`
 *     morphir-elm writes for an untyped value, gives empty attributes. Inside a `Library` or `Bundle` every value,
 *     pattern and value-definition input must be typed, as morphir-elm writes it; a node read on its own need not be.
 *   - A value-definition input's attribute has no slot of its own in the code model; in typed IR it is the input's type
 *     again. An input whose typed attribute names a different type is refused rather than dropped.
 *   - Constructors, `LetRecursion` definitions and `UpdateRecord` fields are ordered by name, word by word; bundle
 *     libraries are ordered by package name. v3 holds each of them in a map, so a repeated name keeps its last entry.
 *   - An entry's documentation string is split into lines; an empty one is no documentation. A module's `doc` is kept
 *     the same way, with `null` or an absent member as no documentation.
 *   - A distribution's package version, which v3 does not record, is left empty.
 *   - Object members a node does not use are ignored.
 *
 * Every method returns a [[V3ReadError]] rather than throwing, with the JSON path of the fault and the Morphir
 * Compatibility Kit's diagnostic code: `invalid_json` when the text is not JSON, `invalid_type` when a node is not the
 * classic JSON of its kind, and the format-version codes for an unrecognised or unsupported `formatVersion`. Each
 * `decode*` method reads one node kind from its JSON text, which is also what a Morphir Compatibility Kit adapter
 * needs.
 */
object V3Reader:

  /** The outcome of reading: the decoded value, or why it could not be read. */
  type Read[A] = Result[V3ReadError, A]

  /** The releases this reader supports, as a canonical support table (finos/morphir `format-version.md`). */
  val SupportTable: String = "[3.0.0,3.2.0)"

  /**
   * A whole `morphir-ir.json` document, `{"formatVersion": …, "distribution": …}`, with the release its `formatVersion`
   * names: one distribution for a `Library` or `Specs`, one library per package for a `Bundle`, ordered by package
   * name.
   */
  def decodeIRDocument(json: String): Read[V3Document] = parsed(json)(V3Decoding.irDocument(_, "$"))

  /** The distributions of a whole `morphir-ir.json` document, as [[decodeIRDocument]] reads them. */
  def decodeIRFile(json: String): Read[Chunk[cm.Distribution]] = decodeIRDocument(json).map(_.distributions)

  /**
   * A bare distribution (`["Library", …]`, `["Specs", …]` or `["Bundle", …]`), without the `formatVersion` envelope.
   * Without the envelope no release is known, so a `Specs` distribution is not checked against one.
   */
  def decodeDistribution(json: String): Read[Chunk[cm.Distribution]] =
    parsed(json)(V3Decoding.distribution(_, "$", Absent))

  /** A `formatVersion` value on its own, such as `3` or `"3.1.0"`, checked against [[SupportTable]]. */
  def decodeFormatVersion(json: String): Read[V3Release] = parsed(json)(V3Decoding.formatVersion(_, "$"))

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

  /** A pattern, such as `["AsPattern",attributes,pattern,name]`, typed or not. */
  def decodePattern(json: String): Read[cm.Pattern] = parsed(json)(V3Decoding.pattern(_, "$"))

  /** A value, such as `["Apply",attributes,function,argument]`, typed or not, as a code-model expression. */
  def decodeValue(json: String): Read[cm.Expr] = parsed(json)(V3Decoding.value(_, "$"))

  /** A type definition: `TypeAliasDefinition` or `CustomTypeDefinition`, whose constructors are ordered by name. */
  def decodeTypeDefinition(json: String): Read[cm.TypeDefinition] = parsed(json)(V3Decoding.typeDefinition(_, "$"))

  /**
   * A type specification: `TypeAliasSpecification`, `OpaqueTypeSpecification`, `CustomTypeSpecification` or
   * `DerivedTypeSpecification`.
   */
  def decodeTypeSpecification(json: String): Read[cm.TypeSpecification] =
    parsed(json)(V3Decoding.typeSpecification(_, "$"))

  /**
   * A value definition, `{"inputTypes": …, "outputType": …, "body": …}`, typed or not. v3 records a value's access on
   * its module entry, not on the definition, so a definition read on its own is public; [[decodeModuleDefinition]]
   * gives each definition the access of its entry.
   */
  def decodeValueDefinition(json: String): Read[cm.ValueDefinition] =
    parsed(json)(V3Decoding.valueDefinition(_, "$"))

  /** A value specification, `{"inputs": …, "output": …}`. */
  def decodeValueSpecification(json: String): Read[cm.ValueSpecification] =
    parsed(json)(V3Decoding.valueSpecification(_, "$"))

  /**
   * A module definition, `{"types": …, "values": …, "doc": …}`, each entry access-controlled and documented. Read on
   * its own, its values may be untyped.
   */
  def decodeModuleDefinition(json: String): Read[cm.ModuleDefinition] =
    parsed(json)(V3Decoding.moduleDefinition(_, "$", V3AttributeRule.TypeOptional))

  /** A module specification, `{"types": …, "values": …, "doc": …}`, each entry documented. */
  def decodeModuleSpecification(json: String): Read[cm.ModuleSpecification] =
    parsed(json)(V3Decoding.moduleSpecification(_, "$"))

  private def parsed[A](json: String)(decode: Structure.Value => Read[A]): Read[A] =
    Json.decode[Structure.Value](json) match
      case Result.Success(value) => decode(value)
      case Result.Failure(error) => Result.fail(V3ReadError.MalformedJson(error.getMessage))
      case Result.Panic(error)   => Result.fail(V3ReadError.MalformedJson(error.getMessage))
end V3Reader
