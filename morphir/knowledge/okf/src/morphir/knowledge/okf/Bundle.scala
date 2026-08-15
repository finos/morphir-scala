package morphir.knowledge.okf

import kyo.*

/** An OKF bundle: a slug, a root index that carries `okf_version`, and the documents it holds. */
final case class Bundle(
    slug: String,
    okfVersion: String,
    index: Concept,
    log: Maybe[Concept] = Absent,
    subIndexes: Chunk[Concept] = Chunk.empty,
    concepts: Chunk[Concept] = Chunk.empty
) derives CanEqual

object Bundle:

  /**
   * Builds a bundle from in-memory markdown files. Keys are bundle-relative paths (`index.md`, `design/foo.md`). The
   * root `index.md` must carry `okf_version`. Filesystem loading is later work.
   */
  def parse(slug: String, files: Map[String, String]): Result[OkfError, Bundle] =
    files.get("index.md") match
      case None              => Result.fail(OkfError.MissingBundleIndex)
      case Some(indexSource) =>
        Concept.parse("index.md", indexSource).flatMap { index =>
          index.frontmatter.okfVersion match
            case Absent =>
              Result.fail(OkfError.InvalidFrontmatter("root index.md must carry okf_version"))
            case Present(version) =>
              parseMembers(files).map { members =>
                val log        = Chunk.from(members.filter(_.kind == DocKind.Log)).headOption
                val subIndexes = Chunk.from(members.filter(_.kind == DocKind.SubIndex))
                val concepts   = Chunk.from(members.filter(_.kind == DocKind.Concept))
                Bundle(
                  slug = slug,
                  okfVersion = version,
                  index = index,
                  log = log.fold(Absent)(Present(_)),
                  subIndexes = subIndexes,
                  concepts = concepts
                )
              }
        }

  private def parseMembers(files: Map[String, String]): Result[OkfError, List[Concept]] =
    files.iterator
      .filter((path, _) => path != "index.md")
      .foldLeft(Result.succeed(List.empty[Concept])) { case (acc, (path, source)) =>
        acc.flatMap { concepts =>
          Concept.parse(path, source).map(concept => concepts :+ concept)
        }
      }
