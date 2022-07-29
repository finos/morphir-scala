package morphir.sdk

import scala.{Int => SInt, List => SList}

object List:
  import morphir.sdk.Int.Int

  opaque type List[+A] = SList[A]

  /**
   * Create a list with only one element
   */
  def singleton[A](value:A):List[A] = SList(value)

  /**
   * Create a list with n copies of a value.
   */
  def repeat[A](n:Int, value:A):List[A] = SList.fill(n)(value)

  def range(lowest:Int, highest:Int):List[Int] = SList.range(lowest, highest)
