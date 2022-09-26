package org.finos
package morphir
package mir

import io.bullet.borer.{Codec, Json}
import zio.test.*
import org.finos.morphir.testing.MorphirBaseSpec
import io.bullet.borer.Cbor
import io.bullet.borer.EncodingSetup.Api
import io.bullet.borer.Cbor.EncodingConfig
import ir.{Name, Path}
import ir.module.ModuleName
import mir.file.format.{MirFile, MirFileFormatVersion, MirFileHeader}

object MirFileSupportSpec extends MorphirBaseSpec {
  import MirFileSupport.given
  val spec = suite("MirFileSupportSpec")(
    suite("org.finos.morphir.ir.Name")(
      test("Supports encoding a Name") {
        val name    = Name.fromString("LocalDate")
        val encoded = Json.encode(name).toUtf8String
        // https://morphir.finos.org/schemas/LocalDate
        assertTrue(
          encoded == """["local","date"]"""
        )
      },
      test("Supports roundtrip CBOR encoding/decoding of a Name") {
        val name                         = Name.fromString("LocalDate")
        val encoded: Api[EncodingConfig] = Cbor.encode(name)
        val encodedBytes                 = encoded.toByteArray
        val decoded                      = Cbor.decode(encodedBytes).to[Name].valueEither
        // https://morphir.finos.org/schemas/LocalDate
        assertTrue(
          decoded == Right(name)
        )
      },
      test("Supports roundtrip JSON encoding/decoding of a Name") {
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
    suite("org.finos.morphir.ir.Path")(
      test("Supports roundtrip CBOR encoding/decoding of a Path") {
        val sut          = Path.fromString("java.lang.String")
        val encoded      = Cbor.encode(sut)
        val encodedBytes = encoded.toByteArray
        val decoded      = Cbor.decode(encodedBytes).to[Path].valueEither
        assertTrue(
          decoded == Right(sut)
        )
      }
    ),
    suite("morphir.mir.ModuleName")(
      test("Supports roundtrip CBOR encoding/decoding of a ModuleName") {
        val sut          = ModuleName(Path.fromString("morphir.sdk"), Name.fromString("Bool"))
        val encoded      = Cbor.encode(sut)
        val encodedBytes = encoded.toByteArray
        val decoded      = Cbor.decode(encodedBytes).to[ModuleName].valueEither
        assertTrue(
          decoded == Right(sut)
        )
      }
    ),
    suite("morphir.mir.file.MirFileFormatVersion")(
      test("Supports roundtrip CBOR encoding/decoding of a MirFileFormatVersion") {
        val sut          = MirFileFormatVersion(2, 13, 9)
        val encoded      = Cbor.encode(sut)
        val encodedBytes = encoded.toByteArray
        val decoded      = Cbor.decode(encodedBytes).to[MirFileFormatVersion].valueEither
        assertTrue(
          decoded == Right(sut)
        )
      }
    ),
    suite("morphir.mir.file.MirFileHeader")(
      test("Supports roundtrip CBOR encoding/decoding of a MirFileHeader") {
        val version      = MirFileFormatVersion(2, 13, 9)
        val moduleName   = ModuleName(Path.fromString("morphir.mir"), Name.fromString("FQName"))
        val sut          = MirFileHeader(version, moduleName)
        val encoded      = Cbor.encode(sut)
        val encodedBytes = encoded.toByteArray
        val decoded      = Cbor.decode(encodedBytes).to[MirFileHeader].valueEither
        assertTrue(
          decoded == Right(sut)
        )
      }
    ),
    suite("morphir.mir.file.MirFile")(
      test("Supports roundtrip CBOR encoding/decoding of a MirFile") {
        val version      = MirFileFormatVersion(0, 1, 2)
        val moduleName   = ModuleName(Path.fromString("morphir.mir"), Name.fromString("QName"))
        val header       = MirFileHeader(version, moduleName)
        val sut          = MirFile(header = header)
        val encoded      = Cbor.encode(sut).withPrintLogging()
        val encodedBytes = encoded.toByteArray
        val decoded      = Cbor.decode(encodedBytes).to[MirFile].valueEither
        assertTrue(
          decoded == Right(sut)
        )
      }
    )
  )
}
