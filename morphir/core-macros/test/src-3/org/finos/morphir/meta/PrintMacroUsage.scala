package org.finos.morphir.meta

object PrintMacroUsage {
  def main(args: Array[String]): Unit =
    println("Running macro")
    PrintMacro {
      trait RocketLauncher {
        def launch(): Unit

      }
    }

    PrintMacro.detail {
      case class Widget(name: String) {}
    }
}
