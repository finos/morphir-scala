package org.finos.morphir.lang.elm.semver
import enumeratum.values._
import zio.Chunk
import zio.prelude._
import zio.prelude.newtypes.Prod

sealed abstract class Strictness(val value: Int) extends Product with Serializable

object Strictness {
  case object Exact  extends Strictness(0)
  case object Safe   extends Strictness(1)
  case object Unsafe extends Strictness(2)

  val values: IndexedSeq[Strictness] = IndexedSeq(Exact, Safe, Unsafe)
}
