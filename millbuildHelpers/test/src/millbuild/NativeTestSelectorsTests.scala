package millbuild

import utest.*

object NativeTestSelectorsTests extends TestSuite {
  val tests = Tests {
    test("selectShard partitions every target exactly once") {
      val resolved = Seq(
        "morphir.prelude.native.test",
        "morphir.langkit.core.native.test",
        "morphir.buildkit.core.native.test",
        "morphir.kit.kyo.native.test",
        "morphir.langkit.markdown.native.test"
      )

      val shards = (0 until 3).map { shard =>
        NativeTestSelectors.selectShard(resolved, shard, 3, "ci.testNative")
      }

      assert(shards.forall(_.isRight))
      val selected = shards.flatMap(_.toOption.toSeq.flatten)
      assert(selected.size == resolved.distinct.size)
      assert(selected.toSet == resolved.toSet)
    }

    test("selectShard is deterministic") {
      val resolved = Seq(
        "morphir.prelude.native.test",
        "morphir.langkit.core.native.test",
        "morphir.buildkit.core.native.test",
        "morphir.langkit.core.native.test"
      )

      val forward = NativeTestSelectors.selectShard(resolved, 0, 2, "ci.testNative")
      val reverse = NativeTestSelectors.selectShard(resolved.reverse, 0, 2, "ci.testNative")

      assert(forward == reverse)
    }

    test("selectShard rejects invalid and empty shards") {
      val resolved = Seq("morphir.prelude.native.test", "morphir.langkit.core.native.test")

      assert(NativeTestSelectors.selectShard(resolved, 0, 0, "ci.testNative").isLeft)
      assert(NativeTestSelectors.selectShard(resolved, -1, 2, "ci.testNative").isLeft)
      assert(NativeTestSelectors.selectShard(resolved, 2, 2, "ci.testNative").isLeft)

      val empty = NativeTestSelectors.selectShard(resolved, 2, 3, "ci.testNative")
      assert(empty.isLeft)
      assert(empty.swap.exists(_.contains("no targets")))
    }
  }
}
