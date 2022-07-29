package morphir
import morphir.runtime.intrinsic

package object interop {

  final class extern extends scala.annotation.StaticAnnotation

  def extern: Nothing = intrinsic
}
