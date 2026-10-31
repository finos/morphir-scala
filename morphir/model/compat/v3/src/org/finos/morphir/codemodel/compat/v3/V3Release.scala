package org.finos.morphir.codemodel.compat.v3

import kyo.Chunk
import kyo.Maybe
import org.finos.morphir.codemodel as cm

/**
 * A release of Morphir IR format version 3, `3.minor.patch`, as a document's `formatVersion` names it. Components are
 * in the unsigned 32-bit range that finos/morphir `docs/spec/ir/format-version.md` sets. A release is not necessarily
 * supported: [[V3Reader.SupportTable]] says which ones the reader reads.
 */
final case class V3Release private (minor: Long, patch: Long) derives CanEqual:

  /** The format family: always 3. */
  def major: Long = 3

  /** `3.0.0`, which a canonical writer spells as the integer `3`. */
  def isBaseline: Boolean = minor == 0 && patch == 0

  /** Whether this release has the `Specs` distribution, which 3.1.0 introduced. */
  def hasSpecs: Boolean = minor >= 1

  /** The canonical `formatVersion` JSON text: `3` for the baseline release, otherwise the quoted release string. */
  def canonicalJson: String = if isBaseline then "3" else s"\"$this\""

  override def toString: String = s"3.$minor.$patch"
end V3Release

object V3Release:
  /** The largest value a release component can have. */
  val MaxComponent: Long = 4294967295L

  /** 3.0.0, the release the integer `3` names. */
  val Baseline: V3Release = new V3Release(0, 0)

  /** 3.1.0, which added the `Specs` distribution and the v3 document tree. */
  val Specs: V3Release = new V3Release(1, 0)

  /** The release `3.minor.patch`, when both components are in range. */
  def of(minor: Long, patch: Long): Maybe[V3Release] =
    if inRange(minor) && inRange(patch) then Maybe(new V3Release(minor, patch)) else Maybe.empty

  private def inRange(component: Long): Boolean = component >= 0 && component <= MaxComponent
end V3Release

/**
 * A whole v3 document: the release its `formatVersion` names and the distributions it holds - one for a `Library` or
 * `Specs`, one per package for a `Bundle`, ordered by package name.
 */
final case class V3Document(release: V3Release, distributions: Chunk[cm.Distribution]) derives CanEqual
