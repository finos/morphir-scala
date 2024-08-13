package org.finos.millmorphir.api
import mill._
import upickle.default.{ReadWriter => RW, readwriter, macroRW}

final case class ArtifactRef(
    pathRef: PathRef,
    artifactType: ArtifactType,
    tags: Set[String] = Set.empty
) { self =>
  def addTag(tag: String): ArtifactRef     = self.copy(tags = tags + tag)
  def addTags(tags: String*): ArtifactRef  = self.copy(tags = self.tags ++ tags)
  def path: os.Path                        = pathRef.path
  def withPath(path: os.Path): ArtifactRef = self.copy(pathRef = PathRef(path))
}

object ArtifactRef {
  implicit val jsonFormatter: RW[ArtifactRef] = macroRW
  def morphirIR(pathRef: PathRef, tags: String*): ArtifactRef =
    ArtifactRef(pathRef, ArtifactType.MorphirIR, Set(tags: _*))
  def morphirHashes(pathRef: PathRef, tags: String*): ArtifactRef =
    ArtifactRef(pathRef, ArtifactType.MorphirHashes, Set(tags: _*))
  def custom(name: String, pathRef: PathRef, tags: String*): ArtifactRef =
    ArtifactRef(pathRef, ArtifactType.Custom(name), Set(tags: _*))
}

sealed abstract class ArtifactType(val tag: String) extends Product with Serializable {
  override def toString: String = tag
}

object ArtifactType {
  implicit val jsonFormatter: RW[ArtifactType] = readwriter[String].bimap(_.tag, fromTag)

  def fromTag(tag: String): ArtifactType = tag match {
    case MorphirIR.tag                  => MorphirIR
    case MorphirHashes.tag              => MorphirHashes
    case _ if tag.startsWith("custom:") => Custom(tag)
    case _                              => throw new IllegalArgumentException(s"Unknown artifact type: $tag")
  }

  case object MorphirIR extends ArtifactType("morphir-ir") { self =>
    implicit val jsonFormatter: RW[MorphirIR.type] = readwriter[String].bimap(_.tag, _ => self)
  }
  case object MorphirHashes extends ArtifactType("morphir-hashes") { self =>
    implicit val jsonFormatter: RW[MorphirHashes.type] = readwriter[String].bimap(_.tag, _ => self)
  }
  case class Custom(name: String) extends ArtifactType(s"custom:$name")
  object Custom {
    implicit val jsonFormatter: RW[Custom] = readwriter[String].bimap(_.tag, Custom(_))
  }
}
