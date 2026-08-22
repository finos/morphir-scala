package morphir.knowledge.okf

import kyo.*
import morphir.langkit.markdown.{MD, MdNode, MdProfile}

/** One markdown file inside an OKF bundle: frontmatter plus a parsed body. */
final case class Concept(
    path: String,
    frontmatter: Frontmatter,
    body: MdNode.Root,
    kind: DocKind = DocKind.Concept,
    hasFrontmatterBlock: Boolean = false
) derives CanEqual

object Concept:

  /**
   * OKF documents carry a leading YAML fence, so the parse that reads them recognizes one.
   *
   * Where the fence ends is a syntax question, and the parser answers it: okf reads `Root.frontmatter` and decodes the
   * raw value it finds there. Owning the split here duplicated a parsing concern and left every body span measured from
   * a stripped string rather than from the file.
   */
  private val profile: MdProfile = MdProfile.commonmark.withYamlFrontmatter

  def parse(path: String, source: String): Result[OkfError, Concept] =
    markdown(source).flatMap { document =>
      val kind = DocKind.of(path)
      document.frontmatter match
        case Absent =>
          Result.succeed(Concept(path, Frontmatter.empty, document, kind, hasFrontmatterBlock = false))
        case Present(MdNode.FrontMatter.Yaml(value, _)) =>
          Frontmatter.parse(value.unwrap).map(fm => Concept(path, fm, document, kind, hasFrontmatterBlock = true))
    }

  private def markdown(source: String): Result[OkfError, MdNode.Root] =
    MD.parser.parse(source)(using profile) match
      case Result.Success(doc) => Result.succeed(doc)
      case Result.Failure(err) => Result.fail(OkfError.Markdown(err))
      case Result.Panic(err)   => Result.Panic(err)
