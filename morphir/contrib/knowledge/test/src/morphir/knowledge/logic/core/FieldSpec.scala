package morphir.knowledge.logic.core

import kyo.test.*

class FieldSpec extends Test[Any]:
  "define is able to get the name from the variable it is being defined on" in {
    val snoop   = Field.define[String]
    val marshal = Field.define[Int]
    val dre     = Field.define[Double]
    assert(snoop.name == "snoop")
    assert(marshal.name == "marshal")
    assert(dre.name == "dre")
  }
end FieldSpec
