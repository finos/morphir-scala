package org.finos.morphir.util.vfile

import java.net.URI

final case class VFilePath private (elements: List[String]) {
  // TODO: This isn't really correct, need to revisit and place correct implementation here
  def /(other: VFilePath): VFilePath = VFilePath(elements ++ other.elements)

  override def toString: String = elements.mkString("/")
}

object VFilePath {
  def userHome: VFilePath                            = fromPathString(System.getProperty("user.home"))
  def apply(first: String, rest: String*): VFilePath = VFilePath(first :: rest.toList)
  def fromPathString(path: String): VFilePath = {
    val separator = sys.props("file.separator")
    VFilePath(path.split(separator).toList)
  }
}
