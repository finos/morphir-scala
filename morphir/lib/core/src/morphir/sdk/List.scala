package morphir.sdk
import org.finos.morphir.interop._

import scala.{List => SList}

@extern
object List:
  import _root_.morphir.sdk.Int.Int

  opaque type List[+A] = SList[A]

  /**
   * Create a list with only one element
   */
  def singleton[A](value: A): List[A] = extern

  /**
   * Create a list with n copies of a value.
   */
  def repeat[A](n: Int, value: A): List[A] = extern

  def range(lowest: Int, highest: Int): List[Int] = extern
