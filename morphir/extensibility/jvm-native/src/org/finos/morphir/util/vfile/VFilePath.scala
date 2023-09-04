package org.finos.morphir.util.vfile

import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters._

final case class VFilePath(path: Path) {
  def elements: List[String]    = path.iterator().asScala.toList.map(_.toString)
  override def toString: String = path.toString()
}

object VFilePath {
  def apply(first: String, rest: String*): VFilePath =
    VFilePath(Paths.get(first, rest: _*))

  def fromJava(path: Path): VFilePath = VFilePath(path)
}
