package morphir.langkit.elm.compiler.mep

import kyo.*
import kyo.test.*

class MepProtocolTests extends Test[Any]:
  "CompileRequest" - {
    "round-trips through Kyo JSON with its MEP field names" in {
      val request = CompileRequest(
        languageId = "elm",
        documents = Chunk(
          SourceDocument("file:///Example.elm", "elm", DocumentVersion(1), "module Example exposing (..)")
        ),
        compilePackage = CompilePackage("local/example", Chunk("Example")),
        dependencies = Chunk.empty,
        options = CompileOptions(typesOnly = false, irVersion = "3")
      )

      val encoded = Json.encode(request)
      val decoded = Json.decode[Structure.Value](encoded).flatMap(Structure.decode[CompileRequest])

      assert(decoded == Result.succeed(request))
      assert(encoded.contains("\"version\":1"))
      assert(encoded.contains("\"package\""))
      assert(!encoded.contains("compilePackage"))
    }

    "models the full unsigned 64-bit document-version contract" in {
      assert(DocumentVersion.Min.toBigInt == BigInt(0))
      assert(DocumentVersion.Max.toBigInt == (BigInt(1) << 64) - 1)
    }

    "round-trips the maximum document version through Kyo Structure without narrowing" in {
      val encoded = Structure.encode(DocumentVersion.Max)

      assert(encoded == Structure.Value.BigNum(BigDecimal(DocumentVersion.Max.toBigInt)))
      assert(Structure.decode[DocumentVersion](encoded) == Result.succeed(DocumentVersion.Max))
    }

    "rejects non-u64 Kyo numeric values" in {
      val invalid = Chunk(
        Structure.Value.Integer(-1),
        Structure.Value.Decimal(1.5),
        Structure.Value.BigNum(BigDecimal("1.5")),
        Structure.Value.BigNum(BigDecimal(DocumentVersion.Max.toBigInt + 1))
      )

      assert(invalid.forall(value => Structure.decode[DocumentVersion](value).isFailure))
    }
  }
end MepProtocolTests
