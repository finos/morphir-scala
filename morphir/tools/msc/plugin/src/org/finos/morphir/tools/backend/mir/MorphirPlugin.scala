package org.finos
package morphir
package tools.backend
package mir

import dotty.tools.dotc.report
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.{PickleQuotes, Staging}

class MorphirPlugin extends StandardPlugin:
  val name: String                 = "morphir"
  override val description: String = "Morphir compiler plugin"
  def init(options: List[String]): List[PluginPhase] =
    // println("===== MorphirPlugin.init =====")
    // println("options: ")
    options.foreach(println)
    val settings = GenMorphirIR.Settings.fromOptions(options)
    // println ("settings: ")
    // println(settings)
    // println("===== MorphirPlugin.init =====")
    List(GenMorphirIR(settings))
