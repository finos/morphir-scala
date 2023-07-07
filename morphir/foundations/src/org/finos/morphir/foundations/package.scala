package org.finos.morphir

package object foundations {
  type Result[+E, +A] = ZResult[Nothing, E, A]
  val Result: ZResult.type = ZResult
}
