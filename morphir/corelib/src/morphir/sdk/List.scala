package morphir.sdk

import scala.{List => ScalaList}

object List:
  opaque type List[+A] = ScalaList[A]

  def singleton[A](value:A):List[A] = ScalaList(value)
