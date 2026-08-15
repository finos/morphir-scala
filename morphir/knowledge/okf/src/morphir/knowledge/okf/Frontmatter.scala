package morphir.knowledge.okf

import kyo.*
import kyo.schema.*

/** A provenance entry from the `sources` frontmatter family. */
final case class SourceRef(
    id: Maybe[String] = Absent,
    resource: String,
    title: Maybe[String] = Absent
) derives CanEqual, Schema

/**
 * Parsed OKF frontmatter. Accessors are permissive: a missing field is [[kyo.Absent]], so a later check can report
 * every problem instead of failing on the first.
 *
 * Field names follow the OKF YAML keys (`stale_after`, `okf_version`) so `kyo-schema-yaml` can decode them without a
 * rename transform. `Schema.rename` needs Tag, and Tag does not yet handle `Maybe`. CamelCase accessors stay for
 * Scala call sites.
 */
final case class Frontmatter(
    `type`: Maybe[String] = Absent,
    title: Maybe[String] = Absent,
    description: Maybe[String] = Absent,
    resource: Maybe[String] = Absent,
    status: Maybe[String] = Absent,
    stale_after: Maybe[String] = Absent,
    okf_version: Maybe[String] = Absent,
    tags: Chunk[String] = Chunk.empty,
    sources: Chunk[SourceRef] = Chunk.empty
) derives CanEqual, Schema:
  def staleAfter: Maybe[String] = stale_after
  def okfVersion: Maybe[String] = okf_version

object Frontmatter:

  val empty: Frontmatter = Frontmatter()

  def parse(raw: String): Result[OkfError, Frontmatter] =
    if raw.trim.isEmpty then Result.succeed(empty)
    else
      Yaml.decode[Frontmatter](raw) match
        case Result.Success(frontmatter) => Result.succeed(frontmatter)
        case Result.Failure(error)       => Result.fail(OkfError.InvalidFrontmatter(error.toString))
        case Result.Panic(error)         => Result.Panic(error)
