package org.finos.morphir.intelligence

import caseapp.*
import caseapp.core.app.{Command, CommandsEntryPoint}
import kyo.*
import kyo.Ansi.*
import org.finos.morphir.intelligence.buildinfo.BuildInfo

final case class VersionOptions(
  @HelpMessage("Output version info as JSON")
  json: Boolean = false
)

final case class VersionInfo(
  version: String,
  scalaVersion: String,
  buildJvmVersion: String,
  buildJvmVendor: String,
  jvmVersion: Option[String],
  jvmVendor: Option[String]
) derives Schema

private def renderTable(rows: Seq[(String, String)]): String =
  val labelWidth = rows.map(_._1.length).max
  val valueWidth = rows.map(_._2.length).max
  val inner      = labelWidth + valueWidth + 3 // " │ "
  val top    = s"╔${"═" * (inner + 2)}╗"
  val bottom = s"╚${"═" * (inner + 2)}╝"
  val sep    = s"╠${"═" * (inner + 2)}╣"
  val lines  = rows.zipWithIndex.map { case ((label, value), i) =>
    val l = label.padTo(labelWidth, ' ').cyan.bold
    val v = value.padTo(valueWidth, ' ').green
    s"║ $l │ $v ║"
  }
  (top +: lines.head +: sep +: lines.tail :+ bottom).mkString("\n")

object VersionCommand extends KyoCommand[VersionOptions]:
  override def name = "version"
  run { (options: VersionOptions) =>
    val info = VersionInfo(
      BuildInfo.version,
      BuildInfo.scalaVersion,
      BuildInfo.buildJvmVersion,
      BuildInfo.buildJvmVendor,
      Option(java.lang.System.getProperty("java.version")),
      Option(java.lang.System.getProperty("java.vendor"))
    )
    if options.json then
      Console.printLine(Json.encode[VersionInfo](info))
    else
      val rows = Seq(
        "version"          -> info.version,
        "scala"            -> info.scalaVersion,
        "jvm (runtime)"    -> info.jvmVersion.getOrElse("unknown"),
        "jvm vendor"       -> info.jvmVendor.getOrElse("unknown"),
        "jvm (build)"      -> info.buildJvmVersion,
        "jvm vendor (build)" -> info.buildJvmVendor
      )
      Console.printLine(renderTable(rows))
  }

object Main extends CommandsEntryPoint:
  def progName                  = "morphir-intelligence"
  def commands: Seq[Command[?]] = Seq(VersionCommand)
