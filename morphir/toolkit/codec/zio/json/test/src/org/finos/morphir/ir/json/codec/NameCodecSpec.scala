package org.finos.morphir
package ir
package json
package codec

import org.finos.morphir.testing.MorphirBaseSpec

import zio.test._
import zio.test.magnolia.DeriveGen
import org.finos.morphir.ir.generator.NameDeriveGen
import java.io.File

object NameJsonCodecProviderSpec extends MorphirJsonBaseSpec with NameDeriveGen with NameJsonCodecProvider {
  def spec = suite("NameJsonCodecProviderSpec")(
    goldenTest(DeriveGen[Name]),
    test("Test root file creation") {
      val rootFile = new File(getClass.getResource("/").toURI)
      println(rootFile.getAbsolutePath)
      assertTrue(rootFile != null)
    }
  )
}
