package org.finos.morphir.intelligence.sdk

import kyo.test.*

class IntentSmokeTest extends Test[Any]:
  "Intent.New round-trips its fields" in {
    val intent = Intent.New("title", "description")
    assert(intent.isInstanceOf[Intent.New])
  }
end IntentSmokeTest
