package org.finos.morphir
import org.finos.morphir.naming.*
import org.finos.morphir.testing.MorphirBaseSpec
import zio.Chunk
import zio.test._

object NamespacingSpec extends MorphirBaseSpec {
  def spec = suite("NamespacingSpec")(
    suite("When qualifying a LocalName")(
      test("Given a LocalName and a Namespace, then a QualifiedName is returned") {
        val localName  = Name("foo")
        val namespace  = Namespace.ns / "bar"
        val pack       = PackageName.root / "baz"
        val qName      = localName.toQualified(pack, namespace)
        val expectedQN = FQName(pack, namespace, localName)
        assertTrue(qName == expectedQN)
      }
    ),
    suite("ToString") {
      test("Given a namespace of foo.bar.baz, then the string representation is Foo.Bar.Baz") {
        val namespace = Namespace.fromStrings("foo", "bar", "baz")
        assertTrue(namespace.toString == "Foo.Bar.Baz")
      }
    }
  )

}
