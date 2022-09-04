package morphir.tools.backend.mir

import scala.annotation.threadUnsafe

import dotty.tools.dotc.core._
import Names._
import Types._
import Contexts._
import Symbols._
import StdNames._
import scala.annotation.{threadUnsafe => tu}
import MirGenUtil.ContextCached

object MirDefinitions {
  private val cached = ContextCached(MirDefinitions())
  def mirDefn(using Context):MirDefinitions = cached.get
}

final class MirDefinitions()(using ctx:Context):
  // Annotations
  @tu lazy val ExternType = requiredClassRef("morphir.interop.extern")
  @tu lazy val ModuleType = requiredClassRef("morphir.module")
  def ExternClass(using Context) = ExternType.symbol.asClass
  def ModuleClass(using Context) = ModuleType.symbol.asClass

end MirDefinitions

  
