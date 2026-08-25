package millbuild

import java.nio.file.{Files, LinkOption, Path}

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

object DesktopSmoke {
  final case class Result(assertions: Map[String, Boolean])

  object Result {
    given upickle.default.ReadWriter[Result] =
      upickle.default.readwriter[Map[String, Boolean]].bimap[Result](_.assertions, Result(_))
  }

  final case class Artifacts(
      screenshot: os.Path,
      result: os.Path,
      processLog: os.Path,
      rendererLog: os.Path
  )

  val expectedAssertions: Set[String] = Set(
    "clearedAfterFailure",
    "clearedAfterSessionSuccess",
    "clearedAfterSuccess",
    "disconnectedThroughButton",
    "mountedRenderer",
    "rememberFalseReadLive",
    "rememberReadLive",
    "rememberTrueReadLive",
    "removedStoredCredentialThroughButton",
    "rendererConsoleSentinelFree",
    "retainedOnFailure",
    "retainedOnSessionSuccess",
    "retainedOnSuccess",
    "safeConnectedStatus",
    "safeRejectedError",
    "safeSessionStatus",
    "submittedThroughForm",
    "transientDomSentinelFree"
  )

  def validate(result: Result): Either[String, Unit] = {
    val actual  = result.assertions.keySet
    val missing = (expectedAssertions -- actual).toSeq.sorted
    val extra   = (actual -- expectedAssertions).toSeq.sorted

    if missing.nonEmpty || extra.nonEmpty then
      Left(
        s"desktop smoke assertion keys differ: missing [${missing.mkString(", ")}]; extra [${extra.mkString(", ")}]"
      )
    else
      result.assertions.iterator.collect { case (name, false) => name }.toSeq.sorted.headOption
        .toLeft(())
        .left
        .map(name => s"desktop smoke assertion failed: $name")
  }

  def redact(value: String, sentinel: String): String =
    if sentinel.isEmpty then value else value.replace(sentinel, "<redacted>")

  def safeRunRoot(base: os.Path, candidate: os.Path): Either[String, os.Path] = {
    val lexicalBase      = base.toNIO.toAbsolutePath.normalize()
    val lexicalCandidate = candidate.toNIO.toAbsolutePath.normalize()

    if lexicalCandidate == lexicalBase then
      Left("desktop smoke run root must be a strict descendant of its base")
    else if !lexicalCandidate.startsWith(lexicalBase) then
      Left("desktop smoke run root must be lexically contained by its base")
    else if Files.isSymbolicLink(lexicalBase) then
      Left("desktop smoke base must not be a symbolic link")
    else if !Files.isDirectory(lexicalBase, LinkOption.NOFOLLOW_LINKS) then
      Left("desktop smoke base must be an existing directory")
    else
      validateComponents(lexicalBase, lexicalCandidate).flatMap { _ =>
        try {
          val physicalBase      = lexicalBase.toRealPath()
          val physicalCandidate = lexicalCandidate.toRealPath()
          validatePhysicalContainment(physicalBase, physicalCandidate)
        } catch {
          case NonFatal(error) =>
            Left(s"cannot resolve desktop smoke run root: ${error.getClass.getSimpleName}")
        }
      }
  }

  private[millbuild] def validatePhysicalContainment(base: Path, candidate: Path): Either[String, os.Path] =
    if candidate == base || !candidate.startsWith(base) then
      Left("desktop smoke run root must be physically contained by its base")
    else Right(os.Path(candidate))

  private def validateComponents(base: Path, candidate: Path): Either[String, Unit] = {
    val components = base.relativize(candidate).iterator().asScala.toSeq
    val paths      = components.scanLeft(base)(_.resolve(_)).tail

    if paths.exists(Files.isSymbolicLink(_)) then
      Left("desktop smoke run root must not traverse a symbolic link")
    else if !Files.isDirectory(candidate, LinkOption.NOFOLLOW_LINKS) then
      Left("desktop smoke run root must be an existing directory")
    else Right(())
  }
}
