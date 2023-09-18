package org.finos.morphir
package ir
package generator

import org.finos.morphir.testing.generators.WordGen
import zio.test.Gen

trait DocumentedGen {
  final def documented[R, A](stringGen: Gen[R, String], dataGen: Gen[R, A]): Gen[R, Documented[A]] = for {
    doc  <- stringGen
    data <- dataGen
  } yield Documented(doc, data)

  final def documentedFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Documented[A]] =
    documented(WordGen.words, attributes)
}

object DocumentedGen extends DocumentedGen
