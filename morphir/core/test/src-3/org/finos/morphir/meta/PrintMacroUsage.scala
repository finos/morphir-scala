package org.finos.morphir.meta

object PrintMacroUsage:
  def greet(target: String): String = s"Hello, $target"

  println("Running macro...")
  val result = PrintMacro.detail {
    greet("42")
  }
  println(result)
