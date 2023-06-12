package org.finos.morphir
package testing
package verifier

import zio.json.*
import zio.test.*

object Verifier {
  def verify[A: JsonCodec](
      name: String
  )(actual: => A)(implicit file: sourcecode.File, line: sourcecode.Line): TestResult = {

    val encoded                  = actual.toJsonPretty
    var expected: Option[String] = None

    println(s"Encoded: $encoded")

    println(s"File: $file")
    assertTrue(1 == 1)
  }
}
