/*
 * Copyright 2023 Morphir Contributors
 * Copyright 2018-2023 John A. De Goes and the ZIO Contributors
 * All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 */
package org.finos.morphir.foundations

/**
 * Ported from ZIO Prelude:
 * https://github.com/zio/zio-prelude/blob/754ced18f21573d1bc128715ceb24899d1e67353/macros/shared/src/main/scala/zio/prelude/AssertionError.scala
 */
sealed trait AssertionError { self =>
  import AssertionError._

  def ++(that: AssertionError): AssertionError =
    (self, that) match {
      case (Many(es1), Many(es2)) => Many((es1 ++ es2).asInstanceOf[::[AssertionError]])
      case (Many(es1), f)         => Many((es1 :+ f).asInstanceOf[::[AssertionError]])
      case (f, Many(es2))         => Many(::(f, es2))
      case (f1, f2)               => Many(::(f1, ::(f2, Nil)))
    }

  def toNel(value: String): ::[String] =
    self match {
      case Failure(condition) =>
        ::(s"$value did not satisfy $condition", Nil)
      case Many(head :: tail) =>
        val f :: fs = head.toNel(value)
        ::(f, fs ++ tail.flatMap(_.toNel(value)))
    }

  def render(value: String): String = self match {
    case Failure(condition) =>
      scala.Console.RED + "• " + scala.Console.BLUE + value + scala.Console.RESET +
        " did not satisfy " + scala.Console.YELLOW + condition + scala.Console.RESET
    case Many(vector) =>
      vector.map(_.render(value)).mkString("\n")
  }
}

object AssertionError {
  def failure(condition: String): AssertionError = Failure(condition)

  final case class Failure(condition: String)       extends AssertionError
  final case class Many(vector: ::[AssertionError]) extends AssertionError
}
