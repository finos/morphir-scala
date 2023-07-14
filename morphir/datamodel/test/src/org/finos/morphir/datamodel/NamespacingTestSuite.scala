package org.finos.morphir.datamodel
import org.finos.morphir.testing.munit.*
import org.finos.morphir.datamodel.namespacing.*
class NamespacingTestSuite extends MorphirTestSuite {
  describe("When qualifying a LocalName") {
    test("Given a LocalName and a Namespace, then a QualifiedName is returned") {
      val localName  = LocalName("foo")
      val namespace  = Namespace.root / NamespaceSegment("bar")
      val qName      = localName.toQualified(namespace)
      val expectedQN = QualifiedName(namespace, localName)
      assertEquals(qName, expectedQN)
    }
  }

}
