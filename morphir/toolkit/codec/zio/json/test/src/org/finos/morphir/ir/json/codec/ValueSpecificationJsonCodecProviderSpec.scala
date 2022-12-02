package org.finos.morphir
package ir
package json
package codec

import java.io.File
import org.finos.morphir.ir.Literal.Literal
import org.finos.morphir.ir.Value
import org.finos.morphir.ir.generator.MorphirIRDeriveGen
import zio.test._
import zio.json.golden.GoldenConfiguration
import zio.test.magnolia.DeriveGen

object ValueSpecificationJsonCodecProviderSpec
    extends MorphirJsonBaseSpec
    with MorphirIRDeriveGen
    with MorphirJsonSupport {
  implicit lazy val givenGoldenConfiguration: GoldenConfiguration =
    GoldenConfiguration.default.copy(
      relativePath = implicitly[sourcecode.FullName].value.split('.').dropRight(1).mkString(File.separator),
      sampleSize = 20
    )
  def spec = suite("ValueSpecificationJsonCodecProviderSpec")(
    goldenTest(DeriveGen[Value.Specification[Literal]])
  )
}
