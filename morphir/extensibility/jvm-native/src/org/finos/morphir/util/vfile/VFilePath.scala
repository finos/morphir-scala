package org.finos.morphir.util.vfile

import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters._

final case class VFilePath(path: Path) {
  def /(other: VFilePath): VFilePath = VFilePath(path.resolve(other.path))
  def elements: List[String]         = path.iterator().asScala.toList.map(_.toString)
  override def toString: String      = path.toString()
}

object VFilePath {
  def userHome: VFilePath = VFilePath(Paths.get(System.getProperty("user.home")))

  def apply(first: String, rest: String*): VFilePath =
    VFilePath(Paths.get(first, rest: _*))

  def fromJava(path: Path): VFilePath = VFilePath(path)
}
