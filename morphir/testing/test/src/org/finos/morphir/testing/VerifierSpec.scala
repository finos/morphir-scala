package org.finos
package morphir
package testing
class VerifierSpec extends MorphirBaseSpec {
  def spec = suite("VerifierSpec")(
    test("Verify should work as expected"){
      Verifier.verify(true)
    }
  )
}
