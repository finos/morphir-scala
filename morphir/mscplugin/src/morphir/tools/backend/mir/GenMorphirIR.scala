package morphir.tools.backend.mir

import dotty.tools._
import dotc._
import plugins._
import core._
import Contexts._
import morphir.tools.backend.mir.config.MorphirSettings

class GenMorphirIR(settings: GenMorphirIR.Settings) extends PluginPhase:
  override val description = GenMorphirIR.description
  override val phaseName   = GenMorphirIR.name

  // override val runsAfter = Set(transform.Pickler.name) //Set(transform.MoveStatics.name)
  override val runsBefore = Set(transform.ElimOpaque.name) // Set(backend.jvm.GenBCode.name)

  override def initContext(ctx: FreshContext): Unit = ()

  override def run(using Context): Unit =
    MirCodeGen(settings).run()

object GenMorphirIR:
  val description = "Generate Morphir IR"
  val name        = "morphir-genMIR"

  case class Settings()
  object Settings:
    def fromOptions(options: List[String]): Settings =
      options.foldLeft(Settings()) { case (config, _) =>
        config
      }
