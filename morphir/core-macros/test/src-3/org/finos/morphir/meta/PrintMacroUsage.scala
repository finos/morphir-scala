package org.finos.morphir.meta

object PrintMacroUsage:
  def greet(target: String): String = s"Hello, $target"

  def main(args: Array[String]): Unit =
    println("Running macro..........")
    val result = PrintMacro.inspect {
      greet("43")

    }
    println(result)
