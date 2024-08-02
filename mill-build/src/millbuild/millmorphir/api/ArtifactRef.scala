package millbuild.millmorphir.api
import mill._
final case class ArtifactRef(
    pathRef: PathRef,
    artifactType: String = "unknown",
    tags: Set[String] = Set.empty
) { self => 
    def addTag(tag: String): ArtifactRef = self.copy(tags = tags + tag)
    def addTags(tags: String*): ArtifactRef = self.copy(tags = self.tags ++ tags)
    def path: os.Path = pathRef.path
    def withPath(path: os.Path): ArtifactRef = self.copy(pathRef = PathRef(path))
}

object ArtifactRef {
    implicit val jsonFormatter: upickle.default.ReadWriter[ArtifactRef] = upickle.default.macroRW
}