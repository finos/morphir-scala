package org.finos.morphir.mill.publish.version

/**
 * The git tag stream an independently versioned area releases through.
 *
 * `None` is the repository's original stream, tagged `v0.6.0-M01`. A namespace prefixes both the tag and the pattern
 * used to find it, so `git describe --match` can be told to look at one stream and ignore the others. That filtering is
 * what keeps a `desktop/v*` tag from becoming the nearest tag for the libraries.
 */
final case class TagStream(namespace: Option[String]) {
  private def prefix: String = namespace.fold("v")(value => s"$value/v")

  /** The `--match` pattern for this stream. */
  def pattern: String = s"$prefix*"

  def tagFor(version: String): String = s"$prefix$version"

  /** The version a tag carries, or None when the tag belongs to another stream. */
  def versionFromTag(tag: String): Option[String] =
    if (!tag.startsWith(prefix)) None
    else {
      val candidate = tag.drop(prefix.length)
      // A bare `v` stream must reject `desktop/v0.3.0`: it does not start with `v`, so the
      // prefix check already excludes it. The slash check excludes the reverse mistake of a
      // namespaced tag being read by a stream whose prefix happens to be a prefix of it.
      if (candidate.contains('/')) None else Some(candidate)
    }
}
