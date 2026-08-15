package morphir.knowledge.okf

import morphir.langkit.markdown.Document

/** One OKF concept: a path inside a bundle, frontmatter, and a parsed markdown body. */
final case class Concept(
    path: String,
    frontmatter: Frontmatter,
    body: Document
) derives CanEqual
