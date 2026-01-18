package org.finos.millmorphir.api

import upickle.default.*

final case class MakeOutputs(moduleId: String, artifacts: Set[ArtifactRef]) derives ReadWriter {
  def addArtifact(artifact: ArtifactRef): MakeOutputs    = MakeOutputs(moduleId, artifacts + artifact)
  def addArtifacts(artifacts: ArtifactRef*): MakeOutputs = MakeOutputs(moduleId, this.artifacts ++ artifacts)
}
