package morphir.knowledge.okf

import kyo.*

/** OKF concept frontmatter. Field names follow the knowledge base conventions, not GitHub's. */
final case class Frontmatter(
    `type`: String,
    title: String,
    description: String,
    tags: Chunk[String]
) derives CanEqual
