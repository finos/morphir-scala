package morphir.ui.services

import kyo.*
import kyo.test.*

class ProtocolTests extends Test[Any]:

  "protocol DTOs" - {

    "ModuleInfo round-trips through JSON" in {
      val m       = ModuleInfo("Morphir.SDK", "List", 4, 21)
      val encoded = Json.encode(m)
      assert(Json.decode[ModuleInfo](encoded) == Result.succeed(m))
    }

    "DefinitionKind round-trips through JSON" in {
      val d = DefinitionDetail(
        DefinitionRef("Morphir.SDK", "List", "map"),
        DefinitionKind.Value,
        "map : (a -> b) -> List a -> List b"
      )
      val encoded = Json.encode(d)
      assert(Json.decode[DefinitionDetail](encoded) == Result.succeed(d))
    }

    "UiServiceError carries its message" in {
      val e: morphir.MorphirException = UiServiceError.WorkspaceNotFound("/tmp/nope")
      assert(e.getMessage.contains("/tmp/nope"))
    }
  }
end ProtocolTests
