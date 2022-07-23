package morphir.mscplugin


import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core
import core.Contexts._
import core.Symbols._
import core.Names._
import dotty.tools.FatalError

import scala.collection.mutable
import scala.language.implicitConversions

class MIRCodeGen(val settings: GenMorphirIR.Settings)(using ctx:Context):
  import tpd._
  def run():Unit = {}
