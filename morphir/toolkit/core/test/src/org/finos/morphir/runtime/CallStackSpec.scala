package org.finos.morphir
package runtime

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*
object CallStackSpec extends MorphirBaseSpec {
  def spec = suite("CallStack Spec")(
    test("An empty CallStack can be created") {
      val sut = CallStack.empty
      assertTrue(sut.frames.isEmpty)
    },
    test("A frame can be pushed onto a CallStack") {
      val sut         = CallStack.empty
      val frame       = StackFrame.empty
      val initialSize = sut.size
      val actual      = sut.push(frame)
      assertTrue(actual.size == 1, actual.size == actual.depth)
    }
  )
}
