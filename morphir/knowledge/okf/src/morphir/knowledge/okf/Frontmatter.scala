package morphir.knowledge.okf

import kyo.*
import kyo.schema.*

/** A provenance entry from the `sources` frontmatter family. */
final case class SourceRef(
    id: Maybe[String] = Absent,
    resource: String,
    title: Maybe[String] = Absent
) derives CanEqual

/**
 * Parsed OKF frontmatter. Accessors are permissive: a missing field is [[kyo.Absent]], so a later check can report
 * every problem instead of failing on the first.
 */
final case class Frontmatter(
    `type`: Maybe[String] = Absent,
    title: Maybe[String] = Absent,
    description: Maybe[String] = Absent,
    resource: Maybe[String] = Absent,
    status: Maybe[String] = Absent,
    @rename("stale_after")
    staleAfter: Maybe[String] = Absent,
    @rename("okf_version")
    okfVersion: Maybe[String] = Absent,
    tags: Chunk[String] = Chunk.empty,
    sources: Chunk[SourceRef] = Chunk.empty
) derives CanEqual

object Frontmatter:

  val empty: Frontmatter = Frontmatter()

  private final case class YamlSourceRef(
      id: Option[String] = None,
      resource: Option[String] = None,
      title: Option[String] = None
  ) derives Schema

  private final case class YamlFrontmatter(
      kind: Option[String] = None,
      title: Option[String] = None,
      description: Option[String] = None,
      resource: Option[String] = None,
      status: Option[String] = None,
      staleAfter: Option[String] = None,
      okfVersion: Option[String] = None,
      tags: List[String] = Nil,
      sources: List[YamlSourceRef] = Nil
  ) derives Schema

  private val yamlSchema: Schema[YamlFrontmatter] =
    Schema[YamlFrontmatter]
      .rename(_.kind, "type")
      .rename(_.staleAfter, "stale_after")
      .rename(_.okfVersion, "okf_version")

  def parse(raw: String): Result[OkfError, Frontmatter] =
    if raw.trim.isEmpty then Result.succeed(empty)
    else
      yamlSchema.decodeString[Yaml](raw) match
        case Result.Success(frontmatter) => Result.succeed(fromYaml(frontmatter))
        case Result.Failure(error)       => Result.fail(OkfError.InvalidFrontmatter(error.toString))
        case Result.Panic(error)         => Result.Panic(error)

  private def fromYaml(frontmatter: YamlFrontmatter): Frontmatter =
    Frontmatter(
      `type` = maybe(frontmatter.kind),
      title = maybe(frontmatter.title),
      description = maybe(frontmatter.description),
      resource = maybe(frontmatter.resource),
      status = maybe(frontmatter.status),
      staleAfter = maybe(frontmatter.staleAfter),
      okfVersion = maybe(frontmatter.okfVersion),
      tags = Chunk.from(frontmatter.tags),
      sources = Chunk.from(frontmatter.sources.flatMap { source =>
        source.resource.map { resource =>
          SourceRef(maybe(source.id), resource, maybe(source.title))
        }
      })
    )

  private def maybe(value: Option[String]): Maybe[String] =
    value.fold(Absent)(Present(_))
