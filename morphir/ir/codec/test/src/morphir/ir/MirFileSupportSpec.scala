package morphir.ir

import io.bullet.borer.{Codec, Json}
import zio.test.*
import morphir.testing.MorphirBaseSpec

object MirFileSupportSpec extends MorphirBaseSpec {
  import MirFileSupport.given
  val spec = suite("MirFileSupportSpec")(
    suite("morphir.ir.Name")(
      test("Supports encoding a Name") {
        val name    = Name.fromString("LocalDate")
        val encoded = Json.encode(name).toUtf8String
        // https://morphir.finos.org/schemas/LocalDate
        assertTrue(
          encoded == """["local","date"]"""
        )
      },
      test("Supports roundtrip encoding/decoding of a Name") {
        val name         = Name.fromString("LocalDate")
        val encoded      = Json.encode(name)
        val encodedStr   = encoded.toUtf8String
        val encodedBytes = encoded.toByteArray
        val decoded      = Json.decode(encodedBytes).to[Name].valueEither
        // https://morphir.finos.org/schemas/LocalDate
        assertTrue(
          encodedStr == """["local","date"]""",
          decoded == Right(name)
        )
      }
    ),
    suite("morphir.ir.Path")(),
    suite("morphir.ir.ModuleName")(),
    suite("morphir.ir.file.MirFileFormatVersion")(),
    suite("morphir.ir.file.MirFileHeader")(),
    suite("morphir.ir.file.MirFile")()
  )
}
