package morphir.knowledge.okf

import morphir.langkit.markdown.MdParseError

/** A failure while parsing an OKF document or bundle. */
enum OkfError derives CanEqual:
  case InvalidFrontmatter(message: String)
  case Markdown(error: MdParseError)
  case MissingBundleIndex
