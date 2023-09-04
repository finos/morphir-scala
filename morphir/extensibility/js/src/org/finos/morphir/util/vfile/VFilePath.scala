package org.finos.morphir.util.vfile

import java.net.URI

final case class VFilePath private (elements: List[String]) {
  override def toString: String = elements.mkString("/")
}

object VFilePath {
  def apply(first: String, rest: String*): VFilePath = VFilePath(first :: rest.toList)
}
