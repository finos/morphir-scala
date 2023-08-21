package org.finos.morphir.datamodel

import org.finos.morphir.naming._
import org.finos.morphir.datamodel.{*, given}
import org.finos.morphir.datamodel.Concept.Enum
import org.finos.morphir.datamodel.Data
import org.finos.morphir.datamodel.Data.Case
import org.finos.morphir.datamodel.Util.*

class ToDataEither extends munit.FunSuite {

  val eitherConcept = Concept.Result(Concept.String, Concept.Int32)
  val rightConcept  = Concept.Result(Concept.String, Concept.Nothing)
  val leftConcept   = Concept.Result(Concept.Nothing, Concept.Int32)

  test("Derive Either (right)") {
    val result = Deriver.gen[Either[String, Int]].derive(Right(123))
    assertEquals(result, Data.Result.Ok(Data.Int(123), eitherConcept))
  }

  test("Derive Either (left)") {
    val result = Deriver.gen[Either[String, Int]].derive(Left("some error"))
    assertEquals(result, Data.Result.Err(Data.String("some error"), eitherConcept))
  }

  test("Derive Right") {
    val result = Deriver.gen[Right[String, Int]].derive(Right(123))
    assertEquals(result, Data.Result.Ok(Data.Int(123), eitherConcept))
  }

  test("Derive Left") {
    val result = Deriver.gen[Left[String, Int]].derive(Left("some error"))
    assertEquals(result, Data.Result.Err(Data.String("some error"), eitherConcept))
  }
}
