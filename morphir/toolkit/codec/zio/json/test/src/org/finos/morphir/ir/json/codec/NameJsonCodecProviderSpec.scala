package org.finos.morphir
package ir
package json
package codec

import org.finos.morphir.testing.MorphirBaseSpec

import zio.test._
import zio.test.magnolia.DeriveGen
import org.finos.morphir.ir.generator.NameDeriveGen
import java.io.File
import zio.json.golden.GoldenConfiguration

object NameJsonCodecProviderSpec extends MorphirJsonBaseSpec with NameDeriveGen with NameJsonCodecProvider {
  implicit lazy val givenGoldenConfiguration: GoldenConfiguration =
    GoldenConfiguration.default.copy(relativePath =
      implicitly[sourcecode.FullName].value.split('.').dropRight(1).mkString(File.separator)
    )
  def spec = suite("NameJsonCodecProviderSpec")(
    goldenTest(DeriveGen[Name])
  )
}
