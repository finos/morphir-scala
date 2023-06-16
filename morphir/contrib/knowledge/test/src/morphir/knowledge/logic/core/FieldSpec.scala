package morphir.knowledge.logic.core
import zio.*
import munit.* 
import com.eed3si9n.expecty.Expecty.expect

class FieldSpec extends munit.ScalaCheckSuite {
  test("define is able to get the name from the variable it is being defined on") {
    val snoop   = Field.define[String]
    val marshal = Field.define[Int]
    val dre     = Field.define[Double]
    expect(snoop.name == "snoop", marshal.name == "marshal", dre.name == "dre")
  }
}
