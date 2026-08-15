package morphir.knowledge.okf

import kyo.*

/** An OKF bundle: a slug and the concepts it holds. */
final case class Bundle(
    slug: String,
    concepts: Chunk[Concept]
) derives CanEqual
