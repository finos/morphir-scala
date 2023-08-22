package org.finos.morphir.interop
import org.finos.morphir.runtime

final class extern extends scala.annotation.StaticAnnotation
object extern {
  def apply: Nothing = runtime.intrinsic
}
