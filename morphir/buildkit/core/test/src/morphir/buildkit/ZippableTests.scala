package morphir.buildkit

import kyo.*
import kyo.test.*
import morphir.Zippable

class ZippableTests extends Test[Any]:

  "Zippable" - {
    "pairs two scalars" in {
      val z = summon[Zippable[Int, String]]
      assert(z.zip(1, "a") == (1, "a"))
    }
    "appends a scalar to a tuple" in {
      val z = summon[Zippable[(Int, String), Boolean]]
      assert(z.zip((1, "a"), true) == (1, "a", true))
    }
    "prepends a tuple to a scalar" in {
      val z = summon[Zippable[Int, (String, Boolean)]]
      assert(z.zip(1, ("a", true)) == (1, "a", true))
    }
    "concatenates two tuples" in {
      val z = summon[Zippable[(Int, String), (Boolean, Long)]]
      assert(z.zip((1, "a"), (true, 2L)) == (1, "a", true, 2L))
    }
    "flattens across a chain" in {
      val z1 = summon[Zippable[Int, String]]
      val z2 = summon[Zippable[(Int, String), Boolean]]
      assert(z2.zip(z1.zip(1, "a"), true) == (1, "a", true))
    }
    "output types are exact" in {
      summon[Zippable.Aux[Int, String, (Int, String)]]
      summon[Zippable.Aux[(Int, String), Boolean, (Int, String, Boolean)]]
      summon[Zippable.Aux[(Int, String), (Boolean, Long), (Int, String, Boolean, Long)]]
      assert(true)
    }
  }
