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
 * Snake-case OKF keys map through `@rename`. `-Yretain-trees` is off by default (opt in with `MorphirRetainTrees`)
 * so `Tag[Maybe[A]]` works (see https://github.com/getkyo/kyo/issues/1883).
 */
final case class Frontmatter(
    `type`: Maybe[String] = Absent,
    title: Maybe[String] = Absent,
    description: Maybe[String] = Absent,
    resource: Maybe[String] = Absent,
    status: Maybe[String] = Absent,
    @rename("stale_after") staleAfter: Maybe[String] = Absent,
    @rename("okf_version") okfVersion: Maybe[String] = Absent,
    tags: Chunk[String] = Chunk.empty,
    sources: Chunk[SourceRef] = Chunk.empty
) derives CanEqual, Schema

object Frontmatter:

  val empty: Frontmatter = Frontmatter()

  def parse(raw: String): Result[OkfError, Frontmatter] =
    if raw.trim.isEmpty then Result.succeed(empty)
    else
      Yaml.decode[Frontmatter](raw) match
        case Result.Success(frontmatter) => Result.succeed(frontmatter)
        case Result.Failure(error)       => Result.fail(OkfError.InvalidFrontmatter(error.toString))
        case Result.Panic(error)         => Result.Panic(error)
