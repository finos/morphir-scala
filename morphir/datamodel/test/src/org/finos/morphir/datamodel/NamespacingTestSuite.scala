package org.finos.morphir.datamodel
import org.finos.morphir.testing.munit.*
import org.finos.morphir.datamodel.namespacing.*
import org.finos.morphir.foundations.Chunk

class NamespacingTestSuite extends MorphirTestSuite {
  describe("When qualifying a LocalName") {
    test("Given a LocalName and a Namespace, then a QualifiedName is returned") {
      val localName  = LocalName("foo")
      val namespace  = Namespace(Chunk.single(NamespaceSegment("bar")))
      val pack       = PackageName(Chunk.single(PackageSegment("baz")))
      val qName      = localName.toQualified(pack, namespace)
      val expectedQN = QualifiedName(pack, namespace, localName)
      assertEquals(qName, expectedQN)
    }
  }

}
