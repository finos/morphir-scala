package org.finos.morphir.launcher
import mainargs.{main, arg, ParserForMethods, Flag, Leftover}
object Main {
  @main
  def run(rest: Leftover[String]) = {
    println("TODO: Implement")
    println(s"rest: ${rest.value}")
  }
  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
