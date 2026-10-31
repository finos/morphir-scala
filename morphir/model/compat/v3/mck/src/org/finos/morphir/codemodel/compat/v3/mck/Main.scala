package org.finos.morphir.codemodel.compat.v3.mck

import java.io.{BufferedReader, FileDescriptor, FileOutputStream, InputStreamReader, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8

import scala.jdk.CollectionConverters.*

/**
 * Starts the adapter on standard input and output. Standard output carries protocol lines only: each answer is written
 * as one line and flushed, because the runner waits for it before sending the next request.
 */
object Main:
  def main(args: Array[String]): Unit =
    val input  = new BufferedReader(new InputStreamReader(System.in, UTF_8))
    val output = new PrintStream(new FileOutputStream(FileDescriptor.out), false, UTF_8)
    V3MckAdapter.serve(input.lines().iterator().asScala) { answer =>
      output.print(answer)
      output.print('\n')
      output.flush()
    }
end Main
