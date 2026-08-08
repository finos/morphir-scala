package org.finos.morphir.mill.elm.morphir

import java.nio.file.{Files, LinkOption}

import mill.*
import org.finos.morphir.mill.MorphirProjectConfig
import org.finos.morphir.mill.elm.{ElmInputLimits, ElmProjectSnapshot}
import upickle.default.{ReadWriter, read}

final case class TrackedMorphirElmProject(
    morphirJson: ElmProjectSnapshot.TrackedElmInput,
    elmJson: Option[ElmProjectSnapshot.TrackedElmInput],
    source: ElmProjectSnapshot.TrackedElmInput
) derives ReadWriter

trait MorphirElmProjectInputsModule extends Module {
  def morphirInputLimits: ElmInputLimits = ElmInputLimits()

  def morphirProjectConfigPath: os.Path = moduleDir / "morphir.json"

  def elmProjectConfigPaths: Seq[os.Path] = Seq(moduleDir / "elm.json")

  def morphirProjectSourcePath: os.Path = moduleDir / "src"

  final def trackedMorphirProjectInputs: T[TrackedMorphirElmProject] = Task.Input {
    val elmPaths = elmProjectConfigPaths.filter(path => Files.exists(path.toNIO, LinkOption.NOFOLLOW_LINKS))
    if (elmPaths.size > 1)
      throw new IllegalArgumentException(
        s"Morphir Elm project supports at most one elm.json input, found: ${elmPaths.mkString(", ")}"
      )
    MorphirElmProjectInputs.capture(
      morphirProjectConfigPath,
      elmPaths.headOption,
      morphirProjectSourcePath,
      morphirInputLimits
    )
  }

  final def morphirProjectConfigFile: T[PathRef] = Task {
    trackedMorphirProjectInputs().morphirJson.pathRef
  }

  final def elmProjectConfigFiles: T[Seq[PathRef]] = Task {
    trackedMorphirProjectInputs().elmJson.toSeq.map(_.pathRef)
  }

  final def morphirProjectSource: T[PathRef] = Task {
    trackedMorphirProjectInputs().source.pathRef
  }

  def morphirProjectConfig: T[MorphirProjectConfig] = Task {
    read[MorphirProjectConfig](os.read(trackedMorphirProjectInputs().morphirJson.pathRef.path))
  }
}

object MorphirElmProjectInputs {
  def capture(
      morphirJson: os.Path,
      elmJson: Option[os.Path],
      source: os.Path,
      limits: ElmInputLimits
  ): TrackedMorphirElmProject = {
    val project = ElmProjectSnapshot.trackInputs(morphirJson, Seq(source), limits)
    val config  = project.find(_.role == ElmProjectSnapshot.InputRole.ElmJson).getOrElse {
      throw invalid("tracked project inputs do not contain morphir.json")
    }
    val sourceInput = project.find(_.role == ElmProjectSnapshot.InputRole.Source).getOrElse {
      throw invalid("tracked project inputs do not contain the Morphir source root")
    }
    val elmInput = elmJson.map { path =>
      ElmProjectSnapshot
        .trackInputs(path, Seq.empty, limits)
        .find(_.role == ElmProjectSnapshot.InputRole.ElmJson)
        .getOrElse(throw invalid("tracked project inputs do not contain elm.json"))
    }
    TrackedMorphirElmProject(config, elmInput, sourceInput)
  }

  def revalidate(expected: TrackedMorphirElmProject, limits: ElmInputLimits): Unit = {
    val actual = capture(
      expected.morphirJson.pathRef.path,
      expected.elmJson.map(_.pathRef.path),
      expected.source.pathRef.path,
      limits
    )
    requireFingerprints(expected, actual, "changed after its verified snapshot")
  }

  def verifyCopied(
      expected: TrackedMorphirElmProject,
      morphirJson: os.Path,
      elmJson: Option[os.Path],
      source: os.Path,
      limits: ElmInputLimits
  ): Unit = {
    val copied = capture(morphirJson, elmJson, source, limits)
    requireFingerprints(expected, copied, "changed while creating its private snapshot")
  }

  private def requireFingerprints(
      expected: TrackedMorphirElmProject,
      actual: TrackedMorphirElmProject,
      reason: String
  ): Unit = {
    if (expected.morphirJson.fingerprint != actual.morphirJson.fingerprint)
      throw invalid(s"morphir.json $reason")
    if (expected.elmJson.map(_.fingerprint) != actual.elmJson.map(_.fingerprint))
      throw invalid(s"elm.json $reason")
    if (expected.source.fingerprint != actual.source.fingerprint)
      throw invalid(s"Morphir source $reason")
  }

  private def invalid(detail: String): IllegalArgumentException =
    new IllegalArgumentException(s"Morphir Elm project input $detail")
}
