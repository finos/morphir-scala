package morphir.spikes.wasmhost.endive

import java.nio.file.Path
import java.util.List as JList
import morphir.spikes.wasmhost.*
import run.endive.runtime.{HostFunction, ImportValues, Instance, WasmFunctionHandle}
import run.endive.wasm.Parser
import run.endive.wasm.types.{FunctionType, ValType}
import kyo.*

object Main extends HostCli(EndiveBackend)

private[wasmhost] object EndiveBackend extends Backend:
  def name                                             = "endive"
  def open(path: Path, outputLimit: Int): GuestSession =
    val bytes   = GuestFiles.read(path)
    val surface = WasmSurface.inspect(bytes)
    if surface.imports.exists(_.module == "extism:host/env") then return extism(bytes, surface, outputLimit)
    WasmSurface.requireBytes(bytes)
    val callbacks = ByteCallbacks(outputLimit)
    def function(name: String, params: JList[ValType], results: JList[ValType])(
        f: Seq[Long] => Array[Long]
    ): HostFunction =
      HostFunction(
        "morphir_bytes_v1",
        name,
        FunctionType.of(params, results),
        new WasmFunctionHandle:
          def apply(instance: Instance, args: Long*): Array[Long] = f(args)
      )
    val imports = ImportValues.builder().addFunction(
      function("input_len", JList.of(), JList.of(ValType.I32))(_ => Array(callbacks.inputLength.toLong)),
      function("input_byte", JList.of(ValType.I32), JList.of(ValType.I32))(args =>
        Array(callbacks.inputByte(args(0).toInt).toLong)
      ),
      function("output_byte", JList.of(ValType.I32), JList.of())(args =>
        callbacks.outputByte(args(0).toInt)
        Array.emptyLongArray
      )
    ).build()
    val instance = Instance.builder(Parser.parse(bytes)).withImportValues(imports)
      .withMemoryFactory(limits =>
        new run.endive.runtime.ByteBufferMemory(
          new run.endive.wasm.types.MemoryLimits(limits.initialPages(), math.min(4096, limits.maximumPages()))
        )
      ).build()
    new GuestSession:
      override def guestMemoryBytes: Maybe[Long] = memoryBytes(instance)
      def answer                                 = instance.`export`("answer").apply()(0).toInt
      def handle(input: Array[Byte]): GuestReply =
        callbacks.begin(input)
        callbacks.finish(instance.`export`("handle").apply()(0).toInt)
      def close(): Unit = instance.close()

  private def extism(bytes: Array[Byte], surface: WasmSurface.Surface, outputLimit: Int): GuestSession =
    val kernel = ExtismKernel(outputLimit)
    kernel.admit(surface)
    def valueType(value: Int): ValType = value match
      case 0x7f => ValType.I32
      case 0x7e => ValType.I64
      case _    => throw IllegalArgumentException("unsupported Extism import value type")
    val functions = surface.imports.map { entry =>
      val signature = surface.types(entry.typeIndex.toInt)
      val params    = new java.util.ArrayList[ValType]()
      signature.params.foreach { value =>
        val _ = params.add(valueType(value))
      }
      val results = new java.util.ArrayList[ValType]()
      signature.results.foreach { value =>
        val _ = results.add(valueType(value))
      }
      HostFunction(
        entry.module,
        entry.name,
        FunctionType.of(params, results),
        new WasmFunctionHandle:
          def apply(instance: Instance, args: Long*): Array[Long] =
            kernel.invoke(
              entry.module,
              entry.name,
              args,
              new kernel.GuestMemory:
                def read(offset: Int, length: Int): Array[Byte]  = instance.memory().readBytes(offset, length)
                def write(offset: Int, bytes: Array[Byte]): Unit = instance.memory().write(offset, bytes)
            )
      )
    }
    val imports  = ImportValues.builder().addFunction(functions*).build()
    val instance = Instance.builder(Parser.parse(bytes)).withImportValues(imports)
      .withMemoryFactory(limits =>
        new run.endive.runtime.ByteBufferMemory(
          new run.endive.wasm.types.MemoryLimits(limits.initialPages(), math.min(4096, limits.maximumPages()))
        )
      ).build()
    if surface.exports.contains("_initialize") then
      val _ = instance.`export`("_initialize").apply()
    new GuestSession:
      override def abi                           = "extism-kernel-v1"
      override def guestMemoryBytes: Maybe[Long] = memoryBytes(instance)
      def answer                                 = instance.`export`("answer").apply()(0).toInt
      def handle(input: Array[Byte]): GuestReply =
        kernel.begin(input)
        kernel.finish(instance.`export`("handle").apply()(0).toInt)
      def close(): Unit = instance.close()

  private def memoryBytes(instance: Instance): Maybe[Long] =
    try
      val memory = instance.memory()
      if memory == null then Absent else Present(memory.pages().toLong * 65536L)
    catch case scala.util.control.NonFatal(_) => Absent
