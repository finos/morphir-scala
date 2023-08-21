package org.finos.morphir

import scala.annotation.implicitAmbiguous

/**
 * Evidence type `A` is not equal to type `B`.
 *
 * Based on https://github.com/milessabin/shapeless.
 */
abstract class =!=[A, B] extends Serializable

object =!= {
  def unexpected: Nothing = sys.error("Unexpected invocation")

  implicit def neq[A, B]: A =!= B = new =!=[A, B] {}

  @implicitAmbiguous("Cannot prove that ${A} =!= ${A}")
  implicit def neqAmbig1[A]: A =!= A = unexpected
  implicit def neqAmbig2[A]: A =!= A = unexpected
}
