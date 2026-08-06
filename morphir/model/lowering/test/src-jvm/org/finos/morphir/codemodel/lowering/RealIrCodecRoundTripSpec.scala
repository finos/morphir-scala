package org.finos.morphir.codemodel.lowering

import kyo.test.*
import kyo.Result
import kyo.Json.given_Json
import zio.json.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.codemodel.CodeModelCodecs
import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.ir.distribution.Distribution as V3Distribution
import org.finos.morphir.ir.json.MorphirJsonDecodingSupport.*

/**
 * Runs `V3Lowering` and the derived kyo-schema codecs (`CodeModelCodecs`) against real `morphir-elm` compiler output,
 * rather than fixtures the implementers constructed themselves.
 *
 * What this proves:
 *   1. `V3Lowering.lowerDistribution` is total and doesn't throw over 745 KB of real IR - 25 modules (`ExampleModule`,
 *      `IfThenElseTests`, `LambdaTests`, `TypeCheckerTests`, `PatternMatchTests`, ...) with real type and value
 *      definitions.
 *   2. The derived codecs round-trip shapes that real IR actually produces: `decode(encode(lowered)) == lowered`.
 *   3. The lowering doesn't drop or duplicate modules, type definitions or value definitions - a coarse structural
 *      correspondence between the v3 tree and the lowered tree, checked by count rather than by full equality (see the
 *      second test below).
 *
 * What this does NOT prove: (2) does not detect information the lowering itself drops or corrupts on the way from v3 to
 * the code model - both sides of that assertion derive from the same already-lowered value, so a bug in `V3Lowering`
 * that e.g. silently zeroed out a field would round-trip perfectly and this test would not notice. (3) only compares
 * counts, not the definitions' contents - swapping two same-shaped type definitions between modules would still pass.
 * Field-level correspondence between v3 and the code model is covered by `V3LoweringSpec`'s unit tests instead, which
 * assert lowered output against hand-constructed v3 input case by case.
 *
 * The fixture (`morphir-ir2.json`) is genuine `morphir-elm` compiler output for the `evaluator-tests` Elm project -
 * format version 3, a `Library` distribution. It is read from `examples/morphir-elm-projects/evaluator-tests/`, the one
 * tracked copy (see `build.LoweringRealIrFixture` in the root `build.mill`), copied onto this module's resources at
 * build time rather than duplicated under `test/resources`.
 *
 * Despite living under `test/src-jvm` (genuinely JVM-only - the JS test module has no cross-platform equivalent of this
 * suite), the fixture itself is not JVM-only: Mill's `resources` task is `Task.Sources("resources")` and is not
 * platform-remapped, so `morphir.model.lowering.jvm.test.resources` and `...js.test.resources` resolve to the same
 * directory. The fixture sits on the JS test classpath too, even though nothing there currently reads it.
 */
class RealIrCodecRoundTripSpec extends Test[Any]:

  private def loadFixtureJson(): String =
    val stream = getClass.getResourceAsStream("/morphir-ir2.json")
    require(stream != null, "test resource morphir-ir2.json not found on classpath")
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()

  private def loadV3Fixture(): V3Distribution.Library =
    loadFixtureJson().fromJson[MorphirIRFile] match
      case Right(file) =>
        file.distribution match
          case lib: V3Distribution.Library => lib
          case other => throw new RuntimeException(s"expected a Library distribution, got: $other")
      case Left(err) => throw new RuntimeException(s"failed to decode v3 fixture: $err")

  "lowered v3 IR survives a derived-schema round trip unchanged" in {
    val v3Lib   = loadV3Fixture()
    val lowered = V3Lowering.lowerDistribution(v3Lib)
    val json    = CodeModelCodecs.encodeDistribution(lowered)
    assert(CodeModelCodecs.decodeDistribution(json) == Result.succeed(lowered))
  }

  "the lowered code model has the same module/type/value counts as the v3 source" in {
    val v3Lib   = loadV3Fixture()
    val lowered = V3Lowering.lowerDistribution(v3Lib)

    val v3Modules = v3Lib.packageDef.modules
    assert(v3Modules.nonEmpty)

    lowered match
      case cm.Distribution.Library(lib) =>
        val loweredModules = lib.definition.modules
        assert(loweredModules.size == v3Modules.size)

        val v3TypeCount       = v3Modules.values.map(_.value.types.size).sum
        val v3ValueCount      = v3Modules.values.map(_.value.values.size).sum
        val loweredTypeCount  = loweredModules.values.map(_.value.types.size).sum
        val loweredValueCount = loweredModules.values.map(_.value.values.size).sum

        assert(loweredTypeCount == v3TypeCount)
        assert(loweredValueCount == v3ValueCount)
      case other => assert(false, s"expected cm.Distribution.Library, got: $other")
  }

end RealIrCodecRoundTripSpec
