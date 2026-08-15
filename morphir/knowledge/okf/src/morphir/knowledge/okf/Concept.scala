package morphir.knowledge.okf

import kyo.*
import morphir.langkit.markdown.{Document, Parser}

/** One markdown file inside an OKF bundle: frontmatter plus a parsed body. */
final case class Concept(
    path: String,
    frontmatter: Frontmatter,
    body: Document,
    kind: DocKind = DocKind.Concept,
    hasFrontmatterBlock: Boolean = false
) derives CanEqual

object Concept:

  /**
   * Splits a leading `---` fenced YAML block off the front of a document. No fence yields `(Absent, wholeText)`. An
   * unclosed fence is treated as no frontmatter.
   */
  def splitFrontmatter(text: String): (Maybe[String], String) =
    val normalized = text.replace("\r\n", "\n")
    if !normalized.startsWith("---\n") then (Absent, normalized)
    else
      val rest = normalized.drop(4)
      closingFence(rest) match
        case Present(i) => (Present(rest.take(i)), rest.drop(i).dropWhile(_ != '\n').drop(1))
        case Absent     => (Absent, normalized)

  def parse(path: String, source: String): Result[OkfError, Concept] =
    val (rawFm, body) = splitFrontmatter(source)
    val kind          = DocKind.of(path)
    rawFm match
      case Absent =>
        markdown(body).map(doc => Concept(path, Frontmatter.empty, doc, kind, hasFrontmatterBlock = false))
      case Present(yaml) =>
        Frontmatter.parse(yaml).flatMap { fm =>
          markdown(body).map(doc => Concept(path, fm, doc, kind, hasFrontmatterBlock = true))
        }

  private def markdown(source: String): Result[OkfError, Document] =
    Parser.parse(source) match
      case Result.Success(doc) => Result.succeed(doc)
      case Result.Failure(err) => Result.fail(OkfError.Markdown(err))
      case Result.Panic(err)   => Result.Panic(err)

  private def closingFence(s: String): Maybe[Int] =
    var idx                = 0
    var result: Maybe[Int] = Absent
    var done               = false
    while !done && result.isEmpty do
      val lineEnd = s.indexOf('\n', idx) match
        case -1 => s.length
        case n  => n
      if s.substring(idx, lineEnd).trim == "---" then result = Present(idx)
      else if lineEnd >= s.length then done = true
      else idx = lineEnd + 1
    result
