package morphir.mscplugin

import scala.annotation.threadUnsafe

import dotty.tools.dotc.core._
import Names._
import Types._
import Contexts._
import Symbols._
import StdNames._
import morphir.mscplugin.config.MorphirPlatform


object MirDefinitions {
  def mirdefn(using Context):MirDefinitions = 
    ctx.platform.asInstanceOf[MorphirPlatform].mirDefinitions
}

final class MirDefinitions()(using ctx:Context) {

}
