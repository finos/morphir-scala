//package wheels.enums
//
//import org.finos.morphir.testing.MorphirBaseSpec
//import zio.test._
//
//object EnumSpec extends MorphirBaseSpec {
//  import Examples._
//  def spec = suite("EnumSpec")(
//    test("values") {
//      assertTrue(Foo.values == List(Foo.Bar, Foo.Baz))
//    }
//  )
//}
//
//object Examples {
//  sealed trait Foo
//  object Foo {
//    case object Bar extends Foo
//    case object Baz extends Foo
//
//    val values = Enum.derived[Foo].values
//  }
//}
