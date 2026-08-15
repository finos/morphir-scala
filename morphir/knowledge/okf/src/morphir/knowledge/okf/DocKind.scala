package morphir.knowledge.okf

/**
 * What role a markdown file plays inside a bundle. Only `index.md` and `log.md` are reserved; every other `.md` file is
 * a concept document.
 */
enum DocKind derives CanEqual:
  case RootIndex, SubIndex, Log, Concept

object DocKind:
  def of(path: String): DocKind =
    val segments = path.stripPrefix("/").split('/').filter(_.nonEmpty).toSeq
    segments.lastOption match
      case Some("log.md")   => DocKind.Log
      case Some("index.md") => if segments.length == 1 then DocKind.RootIndex else DocKind.SubIndex
      case _                => DocKind.Concept
