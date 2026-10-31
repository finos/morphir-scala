package org.finos.morphir.codemodel.compat.v3

/**
 * Why a v3 document could not be read. Every case has the JSON path of the fault (`$` is the document root) and the
 * Morphir Compatibility Kit diagnostic code a conforming reader reports for it.
 *
 * The format-version cases follow finos/morphir `docs/spec/ir/format-version.md`: recognition (type, syntax, range)
 * comes first, then the support check against [[V3Reader.SupportTable]]. Each carries the `formatVersion` as written in
 * the document, as JSON text: `1`, `"3.2.0"`.
 */
enum V3ReadError derives CanEqual:
  /** The input is not well-formed JSON. */
  case MalformedJson(details: String)

  /** The document root has no `formatVersion` member. */
  case MissingFormatVersion

  /** The document root has more than one `formatVersion` member. */
  case DuplicateFormatVersion

  /** The `formatVersion` is neither a string nor an unsigned integer. */
  case InvalidFormatVersionType(found: String)

  /** The `formatVersion` string is not an exact `N.minor.patch` release of a family 3 or later, or is the integer 0. */
  case InvalidFormatVersionSyntax(found: String)

  /** A component of the `formatVersion` is above 4294967295. */
  case FormatVersionOutOfRange(found: String)

  /** The `formatVersion` names a family this reader has no release of: 1, 2, or 5 and later. */
  case UnsupportedFormatVersionMajor(found: String)

  /** The `formatVersion` is a version 3 release outside the support table, such as `"3.2.0"`. */
  case UnsupportedFormatVersionMinor(found: String)

  /** The `formatVersion` is a version 4 release: the document is v4 IR, which a v3 reader does not read. */
  case VersionMismatch(found: String)

  /** A `Specs` distribution declares a release below 3.1.0, the release that introduced it. */
  case SpecsBefore31(found: String)

  /** A tagged array names a tag this node kind does not have. */
  case UnknownTag(path: String, tag: String)

  /** A node does not have the shape its kind requires. */
  case WrongShape(path: String, expected: String)

  /** Where in the input the fault is, as a JSON path from the document root. */
  def location: String = this match
    case MalformedJson(_) | MissingFormatVersion => "$"
    case UnknownTag(path, _)                     => path
    case WrongShape(path, _)                     => path
    case _                                       => "$.formatVersion"

  /** The MCK diagnostic code for this fault. */
  def code: String = this match
    case MalformedJson(_)                    => V3ReadError.Codes.InvalidJson
    case MissingFormatVersion                => V3ReadError.Codes.MissingFormatVersion
    case DuplicateFormatVersion              => V3ReadError.Codes.DuplicateFormatVersion
    case InvalidFormatVersionType(_)         => V3ReadError.Codes.InvalidFormatVersionType
    case InvalidFormatVersionSyntax(_)       => V3ReadError.Codes.InvalidFormatVersionSyntax
    case FormatVersionOutOfRange(_)          => V3ReadError.Codes.FormatVersionOutOfRange
    case UnsupportedFormatVersionMajor(_)    => V3ReadError.Codes.UnsupportedFormatVersionMajor
    case UnsupportedFormatVersionMinor(_)    => V3ReadError.Codes.UnsupportedFormatVersionMinor
    case VersionMismatch(_)                  => V3ReadError.Codes.VersionMismatch
    case SpecsBefore31(_)                    => V3ReadError.Codes.SpecsBefore31
    case UnknownTag(_, _) | WrongShape(_, _) => V3ReadError.Codes.InvalidType

  /** A one-line description of the fault, starting with its location where it has one. */
  def message: String = this match
    case MalformedJson(details)          => s"not well-formed JSON: $details"
    case MissingFormatVersion            => "$: the document has no formatVersion member"
    case DuplicateFormatVersion          => "$: the document has more than one formatVersion member"
    case InvalidFormatVersionType(found) =>
      s"formatVersion $found is not a release string or an unsigned integer"
    case InvalidFormatVersionSyntax(found) =>
      s"formatVersion $found is not an exact N.minor.patch release of format family 3 or later"
    case FormatVersionOutOfRange(found) =>
      s"formatVersion $found has a component above 4294967295"
    case UnsupportedFormatVersionMajor(found) =>
      s"formatVersion $found is not supported; this reader supports ${V3Reader.SupportTable}. " +
        "Regenerate the IR with current morphir-elm."
    case UnsupportedFormatVersionMinor(found) =>
      s"formatVersion $found is a later version 3 release; this reader supports ${V3Reader.SupportTable}"
    case VersionMismatch(found) =>
      s"formatVersion $found is version 4 IR; this reader reads version 3 only"
    case SpecsBefore31(found) =>
      s"formatVersion $found is below 3.1.0, the release that introduced the Specs distribution"
    case UnknownTag(path, tag)      => s"$path: unknown tag '$tag'"
    case WrongShape(path, expected) => s"$path: expected $expected"
end V3ReadError

object V3ReadError:
  /** The MCK diagnostic codes the v3 reader reports. */
  object Codes:
    val InvalidJson                   = "invalid_json"
    val MissingFormatVersion          = "missing_format_version"
    val DuplicateFormatVersion        = "duplicate_format_version"
    val InvalidFormatVersionType      = "invalid_format_version_type"
    val InvalidFormatVersionSyntax    = "invalid_format_version_syntax"
    val FormatVersionOutOfRange       = "format_version_out_of_range"
    val UnsupportedFormatVersionMajor = "unsupported_format_version_major"
    val UnsupportedFormatVersionMinor = "unsupported_format_version_minor"
    val VersionMismatch               = "version_mismatch"
    val SpecsBefore31                 = "specs_before_3_1"

    /** A node that is not the classic JSON of its kind: an unknown tag or a wrong shape. */
    val InvalidType = "invalid_type"
end V3ReadError
