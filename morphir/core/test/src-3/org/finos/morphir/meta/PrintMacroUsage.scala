package org.finos.morphir.meta

object PrintMacroUsage:
  def greet(target: String): String = s"Hello, $target"

  println("Running macro...")
  val result = PrintMacro.detailed {
    greet("42")
  }
  println(result)
