package org.finos.morphir.runtime.parsing

import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.runtime.MorphirRuntimeError.MorphirIRDecodingError
import org.finos.morphir.runtime.fixtures.GeneratedRuntimeFixtures
import org.finos.morphir.testing.MorphirBaseSpec
import zio.ZIO
import zio.json.*
import zio.test.*

object ParseSpec extends MorphirBaseSpec {
  def spec = suite("Morphir IR parsing")(
    test("generated evaluator IR survives a JSON codec round trip") {
      for {
        fileContents <- ZIO.readFile(GeneratedRuntimeFixtures.evaluator.toString)
        original     <- ZIO
          .fromEither(fileContents.fromJson[MorphirIRFile])
          .mapError(MorphirIRDecodingError(_))
        encoded = original.toJson
        decoded <- ZIO
          .fromEither(encoded.fromJson[MorphirIRFile])
          .mapError(MorphirIRDecodingError(_))
      } yield assertTrue(decoded == original)
    }
  )
}
