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

      assert(Json.decode[CompileRequest](encoded) == Result.succeed(request))
      assert(encoded.contains("\"version\":1"))
      assert(encoded.contains("\"package\""))
      assert(!encoded.contains("compilePackage"))
    }

    "models the full unsigned 64-bit document-version contract" in {
      assert(DocumentVersion.Min.toBigInt == BigInt(0))
      assert(DocumentVersion.Max.toBigInt == (BigInt(1) << 64) - 1)
    }
  }
end MepProtocolTests
