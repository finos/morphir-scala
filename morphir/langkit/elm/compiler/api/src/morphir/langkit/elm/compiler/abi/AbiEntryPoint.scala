package morphir.langkit.elm.compiler.abi

import java.nio.charset.StandardCharsets

object AbiEntryPoint:

  private val utf8 = StandardCharsets.UTF_8

  def invokeUtf8(opBytes: Array[Byte], inputBytes: Array[Byte]): Array[Byte] =
    val op    = String(opBytes, utf8)
    val input = String(inputBytes, utf8)
    InvokeCompiler.invoke(op, input).getBytes(utf8)
