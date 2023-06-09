package org.finos.morphir.testing

import org.finos.morphir.testing.verifier.Verifier
import zio.test.*
import zio.{test as _, *}
import zio.test.*
import zio.json.*
trait SnapshotSpec { self: ZIOSpecDefault =>
  def snapshotTest[A: JsonCodec](name: String)(actual: => A)(implicit file: sourcecode.File, line: sourcecode.Line) =
    test(name) {
      Verifier.verify(name)(actual)
    }
}
