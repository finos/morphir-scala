package morphir.langkit.trees

import kyo.test.*

class TreesSpec extends Test[Any]:

  "Trees" - {
    "module marker is set" in
      assert(Trees.moduleName == "morphir-langkit-trees")
  }
