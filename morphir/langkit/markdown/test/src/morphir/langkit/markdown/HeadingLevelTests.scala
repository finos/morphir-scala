package morphir.langkit.markdown

import kyo.*
import kyo.test.*

class HeadingLevelTests extends Test[Any]:

  "HeadingLevel.fromInt" - {
    "accepts the six CommonMark levels" in
      Chunk(1, 2, 3, 4, 5, 6).foreach { n =>
        assert(HeadingLevel.fromInt(n).map(_.toInt) == Present(n))
      }
    "rejects a level below one" in
      assert(HeadingLevel.fromInt(0) == Absent)
    "rejects a level above six" in
      assert(HeadingLevel.fromInt(7) == Absent)
  }

  "HeadingLevel constants" - {
    "carry the level they name" in {
      assert(HeadingLevel.One.toInt == 1)
      assert(HeadingLevel.Six.toInt == 6)
    }
  }
