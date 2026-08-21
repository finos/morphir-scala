package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span

class MdcMetaTests extends Test[Any]:

  private val weight  = MetaKey[Int]("weight")
  private val label   = MetaKey[String]("label")
  private val weight2 = MetaKey[Int]("weight")

  "MetaKey" - {
    "identity is name plus type" in {
      assert(weight == weight2)
      assert(weight.hashCode == weight2.hashCode)
      // same name, different type: distinct keys
      assert(!(weight: Any).equals(MetaKey[String]("weight")))
    }
  }

  "MdcMeta" - {
    "empty carries nothing" in {
      assert(MdcMeta.empty.span == Absent)
      assert(MdcMeta.empty.data.isEmpty)
    }
    "at carries a position" in
      assert(MdcMeta.at(Span(3, 4)).span == Present(Span(3, 4)))
    "get and updated are typed and non-destructive" in {
      val meta = MdcMeta.empty.updated(weight, 3).updated(label, "x")
      assert(meta.get(weight) == Present(3))
      assert(meta.get(label) == Present("x"))
      assert(meta.get(MetaKey[String]("weight")) == Absent) // type is part of identity
      assert(MdcMeta.empty.get(weight) == Absent)
      assert(meta.updated(weight, 4).get(weight) == Present(4))
      assert(meta.get(weight) == Present(3))
    }
  }
