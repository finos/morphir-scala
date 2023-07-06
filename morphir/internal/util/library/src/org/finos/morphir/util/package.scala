package org.finos.morphir

package object util {
  type Result[+E, +A] = ZResult[Nothing, E, A]
  val Result: ZResult.type = ZResult
}
