package morphir.langkit.itest

import java.nio.charset.StandardCharsets
import java.util.List as JList

import com.dylibso.chicory.runtime.HostFunction
import com.dylibso.chicory.runtime.Instance
import com.dylibso.chicory.runtime.Store
import com.dylibso.chicory.runtime.WasmFunctionHandle
import com.dylibso.chicory.wabt.Wat2Wasm
import com.dylibso.chicory.wasm.Parser
import com.dylibso.chicory.wasm.types.FunctionType
import com.dylibso.chicory.wasm.types.ValType

import morphir.langkit.elm.compiler.abi.AbiEntryPoint

object ChicorySupportedCompilerHarness:

  private val utf8 = StandardCharsets.UTF_8

  private val opOffset       = 1024
  private val inputOffset    = 4096
  private val outputOffset   = 32768
  private val outputCapacity = 32768

  private val driverWasm =
    Wat2Wasm.parse(
      s"""
         |(module
         |  (import "morphir" "invoke"
         |    (func $$hostInvoke
         |      (param i32 i32 i32 i32 i32 i32)
         |      (result i32)))
         |
         |  (memory (export "memory") 1)
         |
         |  (func (export "invoke")
         |    (param $$opPtr i32)
         |    (param $$opLen i32)
         |    (param $$inputPtr i32)
         |    (param $$inputLen i32)
         |    (result i32)
         |    (call $$hostInvoke
         |      (local.get $$opPtr)
         |      (local.get $$opLen)
         |      (local.get $$inputPtr)
         |      (local.get $$inputLen)
         |      (i32.const $outputOffset)
         |      (i32.const $outputCapacity))))
         |""".stripMargin
    )

  def invoke(op: String, inputJson: String): String =
    val opBytes    = op.getBytes(utf8)
    val inputBytes = inputJson.getBytes(utf8)
    require(
      opBytes.length <= inputOffset - opOffset,
      s"operation name is too large for the Chicory test driver: ${opBytes.length} bytes"
    )
    require(
      inputBytes.length <= outputOffset - inputOffset,
      s"input JSON is too large for the Chicory test driver: ${inputBytes.length} bytes"
    )

    val instance = instantiate()
    val memory   = instance.memory()
    memory.write(opOffset, opBytes)
    memory.write(inputOffset, inputBytes)

    val outputLength = instance
      .`export`("invoke")
      .apply(
        opOffset,
        opBytes.length,
        inputOffset,
        inputBytes.length
      )(0)
      .toInt

    memory.readString(outputOffset, outputLength)

  private def instantiate(): Instance =
    val store = new Store()
    store.addFunction(compilerInvokeHostFunction())
    store.instantiate("morphir-langkit-itest-driver", Parser.parse(driverWasm))

  private def compilerInvokeHostFunction(): HostFunction =
    HostFunction(
      "morphir",
      "invoke",
      FunctionType.of(
        JList.of(ValType.I32, ValType.I32, ValType.I32, ValType.I32, ValType.I32, ValType.I32),
        JList.of(ValType.I32)
      ),
      new WasmFunctionHandle:
        override def apply(instance: Instance, args: Long*): Array[Long] =
          val opPtr       = args(0).toInt
          val opLen       = args(1).toInt
          val inputPtr    = args(2).toInt
          val inputLen    = args(3).toInt
          val outputPtr   = args(4).toInt
          val outputLimit = args(5).toInt

          val memory      = instance.memory()
          val opBytes     = memory.readBytes(opPtr, opLen)
          val inputBytes  = memory.readBytes(inputPtr, inputLen)
          val outputBytes = AbiEntryPoint.invokeUtf8(opBytes, inputBytes)

          if outputBytes.length > outputLimit then
            throw IllegalStateException(
              s"compiler response is too large for the Chicory test driver: ${outputBytes.length} > $outputLimit bytes"
            )

          memory.write(outputPtr, outputBytes)
          Array(outputBytes.length.toLong)
    )
