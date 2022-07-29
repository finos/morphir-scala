package morphir.ld

import zio.test.ZIOSpecDefault
import zio.Scope
import zio.test.Spec
import zio.test.TestEnvironment

object IriSpec extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("IriSpec")(
    // test("Iri.parse") {
    //   val iri = Iri.parse("http://example.com/")
    //   assert(iri.toString)(equalTo("http://example.com/"))
    // }
  )

}
