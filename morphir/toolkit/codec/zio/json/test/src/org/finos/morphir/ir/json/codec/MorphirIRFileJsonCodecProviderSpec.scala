package org.finos.morphir
package ir
package json
package codec

import java.io.File
import org.finos.morphir.ir.generator.MorphirIRDeriveGen
import zio.test._
import zio.json.golden.GoldenConfiguration
import zio.test.magnolia.DeriveGen

object MorphirIRFileJsonCodecProviderSpec extends MorphirJsonBaseSpec with MorphirIRDeriveGen with MorphirJsonSupport {
  implicit lazy val givenGoldenConfiguration: GoldenConfiguration =
    GoldenConfiguration.default.copy(
      relativePath = implicitly[sourcecode.FullName].value.split('.').dropRight(1).mkString(File.separator),
      sampleSize = 10
    )
  def spec = suite("MorphirIRFileJsonCodecProviderSpec")(
    goldenTest(DeriveGen[MorphirIRFile])
  )
}
