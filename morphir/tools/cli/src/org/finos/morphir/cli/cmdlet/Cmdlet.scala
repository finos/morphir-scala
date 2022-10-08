package org.finos.morphir.cli
package cmdlet
import zio._
import zio.Console.printLine
import zio.process.Command

trait Cmdlet[-Args, +E] {
  def run(cmd: Args): ZIO[ZIOAppArgs, E, Any]
}
