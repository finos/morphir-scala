package org.finos.morphir.codemodel.compat.v3

import kyo.Absent
import kyo.Chunk
import kyo.Json
import kyo.Json.given_Json
import kyo.Present
import kyo.Result
import kyo.Structure

/**
 * Reads a `formatVersion` by finos/morphir `docs/spec/ir/format-version.md`: recognition first - the scalar type, the
 * exact release grammar, the 32-bit component range and the family's spelling - then the support check against
 * [[V3Reader.SupportTable]], `[3.0.0,3.2.0)`.
 *
 * A recognised release outside the table is refused by family: version 4 with `version_mismatch` (the document is v4
 * IR, which needs a v4 reader), the historical integers 1 and 2 and the families from 5 up with
 * `unsupported_format_version_major`, and a version 3 release from 3.2.0 up with `unsupported_format_version_minor`.
 */
private[v3] object V3FormatVersion:
  import V3JsonShape.*
  import V3Reader.Read

  private val FormatVersion = "formatVersion"

  /**
   * The release named by a document root's single `formatVersion` member, which may come before or after the others.
   */
  def ofRoot(fields: Chunk[(String, Value)]): Read[V3Release] =
    fields.filter(_._1 == FormatVersion) match
      case found if found.isEmpty   => Result.fail(V3ReadError.MissingFormatVersion)
      case found if found.size == 1 => release(found.head._2)
      case _                        => Result.fail(V3ReadError.DuplicateFormatVersion)

  /** The supported release a `formatVersion` value names. */
  def release(value: Value): Read[V3Release] =
    val found = Json.encode(value)
    value match
      case Structure.Value.Integer(number)                  => family(BigInt(number), 0, 0, found, integer = true)
      case Structure.Value.BigNum(number) if number.isWhole => family(number.toBigInt, 0, 0, found, integer = true)
      case Structure.Value.Str(text)                        => releaseString(text, found)
      case _                                                => Result.fail(V3ReadError.InvalidFormatVersionType(found))

  private val ReleaseGrammar = """(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)""".r

  private def releaseString(text: String, found: String): Read[V3Release] =
    text match
      case ReleaseGrammar(major, minor, patch) =>
        family(BigInt(major), BigInt(minor), BigInt(patch), found, integer = false)
      case _ => Result.fail(V3ReadError.InvalidFormatVersionSyntax(found))

  private def family(major: BigInt, minor: BigInt, patch: BigInt, found: String, integer: Boolean): Read[V3Release] =
    if major < 0 then Result.fail(V3ReadError.InvalidFormatVersionType(found))
    else if Seq(major, minor, patch).exists(_ > V3Release.MaxComponent) then
      Result.fail(V3ReadError.FormatVersionOutOfRange(found))
    else if major == 0 || (major < 3 && !integer) then Result.fail(V3ReadError.InvalidFormatVersionSyntax(found))
    else if major == 3 then
      V3Release.of(minor.toLong, patch.toLong) match
        case Present(release) if release.minor < 2 => Result.succeed(release)
        case Present(_)                            => Result.fail(V3ReadError.UnsupportedFormatVersionMinor(found))
        case Absent                                => Result.fail(V3ReadError.FormatVersionOutOfRange(found))
    else if major == 4 then Result.fail(V3ReadError.VersionMismatch(found))
    else Result.fail(V3ReadError.UnsupportedFormatVersionMajor(found))
end V3FormatVersion
