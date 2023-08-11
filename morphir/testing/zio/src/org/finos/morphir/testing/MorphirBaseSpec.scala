package org.finos.morphir.testing

import zio.internal.stacktracer.SourceLocation
import zio.{test as _, *}
import zio.test.*

import scala.annotation.nowarn

abstract class MorphirBaseSpec extends ZIOSpecDefault {
  override def aspects = Chunk(TestAspect.timeout(90.seconds))

  def tableTest[A, Actual, Expected <: Actual](label: String)(colA: String)(first: (A, Expected), rest: (A, Expected)*)(
      when: A => Actual
  )(implicit sourceLocation: SourceLocation) = {
    def makeTest(row: (A, Expected)): Spec[Any, Nothing] = {
      val (a, expected) = row
      val actual        = when(a)
      test(s"Given $colA is $a, then the result should be $expected") {
        assertTrue(
          actual == expected
        ) ?? s"Given input of $colA=$a"
      }
    }

    val tests = rest.foldLeft(makeTest(first))((acc, row) => acc + makeTest(row))
    suite(label)(tests)
  }

  def expectAllEqual[A, Actual, Expected <: Actual](first: (A, Expected), rest: (A, Expected)*)(
      when: A => Actual
  )(implicit @nowarn sourceLocation: SourceLocation): TestResult = {
    def onRow(row: (A, Expected)) = {
      val (a, expected) = row
      val actual        = when(a)
      assertTrue(actual == expected) ?? s"Given input of $a"
    }
    rest.foldLeft(onRow(first))((acc, row) => acc && onRow(row))
  }

  def expectAll[A, Actual, Expected <: Actual](first: (A, Expected), rest: (A, Expected)*)(
      assert: (A, Expected) => TestResult
  )(implicit @nowarn sourceLocation: SourceLocation): TestResult = {
    def onRow(row: (A, Expected)) = {
      val (a, expected) = row
      assert(a, expected)
    }

    rest.foldLeft(onRow(first))((acc, row) => acc && onRow(row))
  }
}
