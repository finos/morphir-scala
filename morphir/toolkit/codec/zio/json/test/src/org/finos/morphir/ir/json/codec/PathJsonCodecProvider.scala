package org.finos.morphir.ir.json.codec

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._
import zio.test.magnolia.DeriveGen
import org.finos.morphir.ir.generator._
import java.io.File
import zio.json.golden.GoldenConfiguration
import org.finos.morphir.ir.json.MorphirJsonBaseSpec
import org.finos.morphir.ir.Path

object PathJsonCodecSpec extends MorphirJsonBaseSpec with PathDeriveGen with PathJsonCodecProvider {
  implicit lazy val givenGoldenConfiguration: GoldenConfiguration =
    GoldenConfiguration.default.copy(relativePath =
      implicitly[sourcecode.FullName].value.split('.').dropRight(1).mkString(File.separator)
    )
  def spec = suite("PathJsonCodecProviderSpec")(
    goldenTest(DeriveGen[Path])
  )
}
