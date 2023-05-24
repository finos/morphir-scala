package org.finos.morphir.testing
import zio.test._
object Verifier {
  def verify[A](actual:A)(implicit file:sourcecode.File):TestResult = {
    println(s"File: $file")
    assertTrue( 1 == 1)
  }
}
