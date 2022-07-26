package morphir.mscplugin

import dotty.tools.dotc.core
import core.Contexts.*
import core.Flags.*
import core.Symbols.*
import dotty.tools.backend.jvm.DottyBackendInterface.symExtensions
import morphir.ir.FQName
import morphir.ir.Module.ModuleName

object MorphirEncoding:
  def encodeModuleName(sym:Symbol)(using Context):FQName =
    val sym1 =
      if(sym.isAllOf(ModuleClass | JavaDefined)) sym.linkedClass
      else sym
    val moduleName = ModuleName.fromString(sym1.javaClassName)
    FQName.fqn(moduleName)
