package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import zio.test.Gen

trait FieldGen {
  final def field[R, A](nameGen: Gen[R, Name], dataGen: Gen[R, A]): Gen[R, Field[A]] = for {
    name <- nameGen
    data <- dataGen
  } yield Field(name, data)

  final def fieldFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Field[A]] =
    field(NameGen.name, attributes)
}

object FieldGen extends FieldGen
