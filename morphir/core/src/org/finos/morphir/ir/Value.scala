package org.finos.morphir.ir

object Value {
  sealed trait Specification[+TA] {
    def map[B](f: TA => B): Specification[B]
  }
}
