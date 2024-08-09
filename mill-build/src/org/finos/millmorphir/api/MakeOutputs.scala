package org.finos.millmorphir.api

final case class MakeOutputs(moduleId:String, artifacts:Set[ArtifactRef]) {
  def addArtifact(artifact: ArtifactRef): MakeOutputs = MakeOutputs(moduleId, artifacts + artifact)
  def addArtifacts(artifacts: ArtifactRef*): MakeOutputs = MakeOutputs(moduleId, this.artifacts ++ artifacts)
}

object MakeOutputs {
  implicit val jsonFormatter: upickle.default.ReadWriter[MakeOutputs] = upickle.default.macroRW
}
