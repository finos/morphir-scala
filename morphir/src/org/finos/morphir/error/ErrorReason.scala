package org.finos.morphir.error

import scala.annotation.tailrec

sealed trait ErrorReason[+E] { self =>
  import ErrorReason._

  def &&[E1 >: E](that: ErrorReason[E1]): ErrorReason[E1] = Both(self, that)
  def ++[E1 >: E](that: ErrorReason[E1]): ErrorReason[E1] =
    if (self eq Empty) that else if (that eq Empty) self else Then(self, that)

  /// Produces a list of all recoverable errors `E` in the `ErrorReason`.
  final def failures: List[E] =
    self.foldLeft(List.empty[E]) { case (z, Fail(v)) =>
      v :: z
    }
      .reverse

  final def foldLeft[Z](z: Z)(f: PartialFunction[(Z, ErrorReason[E]), Z]): Z = {
    @tailrec
    def loop(z0: Z, reason: ErrorReason[E], stack: List[ErrorReason[E]]): Z = {
      val z = f.applyOrElse[(Z, ErrorReason[E]), Z](z0 -> reason, _._1)
      reason match {
        case Then(left, right)   => loop(z, left, right :: stack)
        case Both(left, right)   => loop(z, left, right :: stack)
        case _ if stack.nonEmpty => loop(z, stack.head, stack.tail)
        case _                   => z
      }
    }
    if (self eq Empty) z
    else loop(z, self, Nil)

  }

  // def flatMap[E2](f: E => ErrorCause[E2]):ErrorCause[E2] = self match {
  //     case Empty =>
  // }
  // def map[E1](f: E => E1):ErrorCause[E1] = ???
}

object ErrorReason {
  val empty: ErrorReason[Nothing]                                          = Empty
  def fail[E](error: E): ErrorReason[E]                                    = Fail(error)
  def both[E](left: ErrorReason[E], right: ErrorReason[E]): ErrorReason[E] = Both(left, right)

  case object Empty                                                      extends ErrorReason[Nothing]
  final case class Fail[+E](error: E)                                    extends ErrorReason[E]
  final case class Both[+E](left: ErrorReason[E], right: ErrorReason[E]) extends ErrorReason[E]
  final case class Then[+E](left: ErrorReason[E], right: ErrorReason[E]) extends ErrorReason[E]
}
