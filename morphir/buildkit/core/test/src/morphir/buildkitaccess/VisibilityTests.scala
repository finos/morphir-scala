package morphir.buildkitaccess

import kyo.*
import kyo.test.*
import morphir.buildkit.*

/**
 * Lives outside `morphir.buildkit` on purpose: proves the public surface is usable from a foreign package, and that the
 * internal implementation types stay out of reach even with `morphir.buildkit.*` imported.
 */
class VisibilityTests extends Test[Any]:

  private def observer = Stage
    .fromKyo[Int, String, Any](i => Pipeline.provenance.map(path => path.map(_.label).mkString(",")))
    .named("observer")

  "Pipeline.provenance" - {
    "is readable from a foreign package through the public reader alone" in {
      val plan = Pipeline.stage(observer).seal match
        case Result.Success(sealed_) => sealed_
        case other                   => throw new AssertionError(s"seal failed: $other")
      val (_, result) = Emit.run(plan.execute(0)).eval
      assert(result == "observer")
    }
  }

  "internal encapsulation" - {
    "morphir.buildkit.internal.NodeChain does not typecheck outside morphir.buildkit" in
      assert(!scala.compiletime.testing.typeChecks(""" (???: morphir.buildkit.internal.NodeChain[Any, Any, Any]) """))
    "NodeId.unsafe does not typecheck outside morphir.buildkit" in
      assert(!scala.compiletime.testing.typeChecks(""" morphir.buildkit.NodeId.unsafe(kyo.Chunk("x")) """))
  }

  "nodeId interpolator" - {
    "expands and typechecks at a foreign-package expansion site" in
      assert(nodeId"outside".render == "outside")
  }
