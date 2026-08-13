package morphir.buildkit

import kyo.*

/**
 * Contextual metadata attached to a [[Stage]] through [[Stage.named]].
 *
 * `label` is a human-facing display name for progress reporting, diagnostics provenance and rendering; it is not an
 * identity, and nothing requires it to be unique. The pipeline graph may default a node's identity from it, but
 * uniqueness is enforced there at seal time, never here.
 */
final case class StageMeta(label: String, description: Maybe[String] = Absent)
