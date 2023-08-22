// This file was taken from/inspired by the source code of the shapely project (https://gitlab.com/fommil/shapely)
package millbuild

object ExamplesCodeGen {
  val product_arity = 64
  val sum_arity = 64

  def enums: String = {
    val enums = (1 to sum_arity).map { i =>
      val tparams = (1 to i).map(p => s"A$p <: A").mkString(", ")
      val tparams_ = (1 to i).map(p => s"A$p").mkString(", ")
      val implicits = (1 to i).map(p => s"A$p: ValueOf[A$p]").mkString(", ")
      val tycons = s"SealedTrait$i[A, $tparams_]"
      val work = (1 to i).map { p => s"(SealedTrait._$p(A$p.value): AA)" }.mkString("", " :: ", " :: Nil")
      s"""  implicit def sealedtrait$i[A, $tparams](implicit $implicits): Enum[$tycons] = new Enum[$tycons] {
         |    private type AA = $tycons
         |    def values: List[$tycons] = $work
         |  }""".stripMargin
    }
    s"""package wheels.enums
       |
       |import org.finos.morphir.meta._
       |
       |private[enums] trait GeneratedEnums {
       |${enums.mkString("\n\n")}
       |}""".stripMargin
  }

}
