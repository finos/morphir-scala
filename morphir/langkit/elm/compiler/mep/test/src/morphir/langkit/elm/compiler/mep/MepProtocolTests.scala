package morphir.langkit.elm.compiler.mep

import kyo.*
import kyo.test.*

class MepProtocolTests extends Test[Any]:
  private val documentsJson =
    """[{"uri":"file:///Example.elm","languageId":"elm","version":1,"text":"module Example exposing (..)"}]"""

  private def decodeCompile(sourceFields: String): Result[String, CompileRequest] =
    CompileRequest.decode(Json.decode[Structure.Value](s"""{
      "languageId": "elm",
      "package": {"name": "local/example", "exposedModules": ["Example"]},
      "dependencies": [],
      "options": {"typesOnly": false, "irVersion": "3", "sourceRootUri": "file:///legacy/", "unknown": true},
      "unknown": true
      $sourceFields
    }""").getOrThrow)

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

    "normalizes both envelopes and ignores roots and unknown members" in {
      val legacy   = decodeCompile(s""", "documents": $documentsJson""")
      val modern   = decodeCompile(s""", "sources": {"documents": $documentsJson, "unknown": true}""")
      val rooted   = decodeCompile(s""", "sources": {"root": "file:///modern/", "documents": $documentsJson}""")
      val expected = CompileRequest(
        "elm",
        Chunk(SourceDocument("file:///Example.elm", "elm", DocumentVersion(1), "module Example exposing (..)")),
        CompilePackage("local/example", Chunk("Example")),
        Chunk.empty,
        CompileOptions(typesOnly = false, irVersion = "3")
      )

      assert(legacy == Result.succeed(expected))
      assert(modern == legacy)
      assert(rooted == legacy)
    }

    "rejects both envelopes even when one is null" in {
      val expected =
        Result.fail("Ambiguous morphir.frontend.compile parameters: provide either sources or documents, not both")
      assert(decodeCompile(s""", "documents": $documentsJson, "sources": {"documents": $documentsJson}""") == expected)
      assert(decodeCompile(s""", "documents": $documentsJson, "sources": null""") == expected)
      assert(decodeCompile(s""", "documents": null, "sources": {"documents": $documentsJson}""") == expected)
    }

    "rejects a missing compile envelope" in
      assert(decodeCompile("") ==
        Result.fail("Invalid morphir.frontend.compile parameters: missing sources or documents"))

    "requires a string for every present sources root" in {
      val invalidRoots = Chunk("null", "1", "false", "{}", "[]")
      invalidRoots.foreach { root =>
        assert(
          decodeCompile(s""", "sources": {"root": $root, "documents": $documentsJson}""") ==
            Result.fail("Invalid morphir.frontend.compile parameters: sources.root must be a string")
        )
      }
    }

    "rejects malformed sources and documents" in {
      val invalidSources = Chunk("null", "1", "[]", "{}", """{"documents":null}""", """{"documents":{}}""")
      invalidSources.foreach { sources =>
        assert(decodeCompile(s""", "sources": $sources""").isFailure)
      }
      assert(decodeCompile(""", "documents": null""").isFailure)
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
