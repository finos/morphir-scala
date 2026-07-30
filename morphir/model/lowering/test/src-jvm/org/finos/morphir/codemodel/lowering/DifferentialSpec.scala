package org.finos.morphir.codemodel.lowering

import kyo.test.*
import kyo.Result
import kyo.Json.given_Json
import zio.json.*
import org.finos.morphir.codemodel.CodeModelCodecs
import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.ir.distribution.Distribution as V3Distribution
import org.finos.morphir.ir.json.MorphirJsonDecodingSupport.*

/**
 * Differential test: the same v3 `morphir-ir.json` decoded two ways must agree.
 *
 *   1. v3 JSON --(zio-json, `MorphirJsonDecodingSupport`)--> v3 `Distribution` --(`V3Lowering`)--> code model
 *   2. that lowered code model --(derived kyo-schema, `CodeModelCodecs`)--> JSON --(derived kyo-schema)--> code model
 *
 * If (1) and (2) disagree, either the lowering drops information or the derived codecs are not round-trip stable - both
 * are things the rest of the stack needs to know about before building on top of it. This is the first time the
 * lowering and the derived schema meet real IR rather than fixtures the implementers constructed themselves.
 *
 * The fixture (`morphir-ir2.json`, JVM-only test resource copied from
 * `examples/morphir-elm-projects/evaluator-tests/morphir-ir2.json`) is genuine `morphir-elm` compiler output for the
 * `evaluator-tests` Elm project - format version 3, a `Library` distribution, 25 modules (`ExampleModule`,
 * `IfThenElseTests`, `LambdaTests`, `TypeCheckerTests`, `PatternMatchTests`, ...) with real type and value definitions,
 * not hand-written or generator-produced data.
 */
class DifferentialSpec extends Test[Any]:

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

end DifferentialSpec
