/*
 * Copyright 2023 Morphir Contributors
 * Copyright 2018-2023 John A. De Goes and the ZIO Contributors
 * All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 */
package org.finos.morphir.foundations

import scala.quoted._

/**
 * Convience type that helps with defining a Newtype with a custom (so anything that is not expressable with an
 * [[Assertion]]) predicate that should be checked during compile-time.
 *
 * Define this in a separate file then the Newtype itself!
 *
 * File 1:
 * {{{
 * import org.finos.morphir.foundations.Validator
 *
 * object PalindromeValidator extends Validator[String](str =>
 *   if (str.reverse == str) Right(()) else Left(AssertionError.failure("isPalindrome"))
 * )
 * }}}
 *
 * File 2:
 * {{{
 * type Palindrome = Palindrome.type
 * object Palindrome extends NewtypeCustom[String] {
 *   protected def validate(value: Int) = PalindromeValidator.validate(value)
 *
 *   protected inline def validateInline(inline value: Int) =
 *    ${ PalindromeValidator.validateInlineImpl('value) }
 * }
 * }}}
 */
abstract class Validator[A: FromExpr](f: A => Either[AssertionError, Unit]) extends Liftables {
  def validate(a: A): Either[AssertionError, Unit] =
    f(a)

  final def validateInlineImpl(expr: Expr[A])(using Quotes): Expr[Unit] = {
    val a = expr.valueOrAbort
    validate(a) match {
      case Right(_) => '{ () }
      case Left(err) =>
        quotes.reflect.report.errorAndAbort(err.render(a.toString))
    }
  }

}
