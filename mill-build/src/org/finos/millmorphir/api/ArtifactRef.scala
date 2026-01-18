package org.finos.millmorphir.api

import mill.*
import upickle.default.{ReadWriter => RW, readwriter}

final case class ArtifactRef(
    pathRef: PathRef,
    artifactType: ArtifactType,
    tags: Set[String] = Set.empty
) derives RW { self =>
  def addTag(tag: String): ArtifactRef     = self.copy(tags = tags + tag)
  def addTags(tags: String*): ArtifactRef  = self.copy(tags = self.tags ++ tags)
  def path: os.Path                        = pathRef.path
  def withPath(path: os.Path): ArtifactRef = self.copy(pathRef = PathRef(path))
}

object ArtifactRef {
  def morphirIR(pathRef: PathRef, tags: String*): ArtifactRef =
    ArtifactRef(pathRef, ArtifactType.MorphirIR, Set(tags*))
  def morphirHashes(pathRef: PathRef, tags: String*): ArtifactRef =
    ArtifactRef(pathRef, ArtifactType.MorphirHashes, Set(tags*))
  def custom(name: String, pathRef: PathRef, tags: String*): ArtifactRef =
    ArtifactRef(pathRef, ArtifactType.Custom(name), Set(tags*))
}

sealed abstract class ArtifactType(val tag: String) extends Product with Serializable {
  override def toString: String = tag
}

object ArtifactType {
  given RW[ArtifactType] = readwriter[String].bimap(_.tag, fromTag)

  def fromTag(tag: String): ArtifactType = tag match {
    case MorphirIR.tag                  => MorphirIR
    case MorphirHashes.tag              => MorphirHashes
    case _ if tag.startsWith("custom:") => Custom(tag)
    case _                              => throw new IllegalArgumentException(s"Unknown artifact type: $tag")
  }

  case object MorphirIR extends ArtifactType("morphir-ir") { self =>
    given RW[MorphirIR.type] = readwriter[String].bimap(_.tag, _ => self)
  }
  case object MorphirHashes extends ArtifactType("morphir-hashes") { self =>
    given RW[MorphirHashes.type] = readwriter[String].bimap(_.tag, _ => self)
  }
  case class Custom(name: String) extends ArtifactType(s"custom:$name")
  object Custom {
    given RW[Custom] = readwriter[String].bimap(_.tag, Custom(_))
  }
}
