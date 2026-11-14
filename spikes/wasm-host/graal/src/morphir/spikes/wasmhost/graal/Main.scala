package morphir.spikes.wasmhost.graal

import java.nio.file.Path
import morphir.spikes.wasmhost.*
import org.graalvm.polyglot.{Context, Source, Value}
import org.graalvm.polyglot.io.ByteSequence
import org.graalvm.polyglot.proxy.{ProxyExecutable, ProxyObject}
import kyo.*

object Main extends HostCli(GraalBackend)

private[wasmhost] object GraalBackend extends Backend:
  def name                                             = "graalwasm"
  def open(path: Path, outputLimit: Int): GuestSession =
    val bytes   = GuestFiles.read(path)
    val surface = WasmSurface.inspect(bytes)
    require(!surface.unboundedMemory, "GraalWasm 25.0.1 requires a declared maximum for the 256 MiB bound")
    if surface.imports.exists(_.module == "extism:host/env") then return extism(bytes, surface, outputLimit)
    WasmSurface.requireBytes(bytes, requireMemoryMaximum = true)
    val callbacks = ByteCallbacks(outputLimit)
    val context   = Context.newBuilder("wasm").build()
    try
      val functions                                            = new java.util.HashMap[String, Object]()
      def executable(f: Seq[Value] => Object): ProxyExecutable = new ProxyExecutable:
        def execute(args: Value*): Object = f(args)
      functions.put("input_len", executable(_ => Integer.valueOf(callbacks.inputLength)))
      functions.put("input_byte", executable(args => Integer.valueOf(callbacks.inputByte(args(0).asInt()))))
      functions.put(
        "output_byte",
        executable(args =>
          callbacks.outputByte(args(0).asInt())
          null
        )
      )
      val imports = new java.util.HashMap[String, Object]()
      imports.put("morphir_bytes_v1", ProxyObject.fromMap(functions))
      val module  = context.eval(Source.newBuilder("wasm", ByteSequence.create(bytes), "guest").build())
      val exports = module.newInstance(ProxyObject.fromMap(imports)).getMember("exports")
      new GuestSession:
        def answer                                 = exports.getMember("answer").execute().asInt()
        def handle(input: Array[Byte]): GuestReply =
          callbacks.begin(input)
          callbacks.finish(exports.getMember("handle").execute().asInt())
        def close(): Unit = context.close()
    catch
      case error: Throwable =>
        context.close(true)
        throw error

  private def extism(bytes: Array[Byte], surface: WasmSurface.Surface, outputLimit: Int): GuestSession =
    val kernel = ExtismKernel(outputLimit)
    kernel.admit(surface)
    val context = Context.newBuilder("wasm").build()
    try
      var active: Maybe[Value] = Absent
      val imports              = new java.util.HashMap[String, Object]()
      surface.imports.groupBy(_.module).foreach { (namespace, entries) =>
        val functions = new java.util.HashMap[String, Object]()
        entries.foreach { entry =>
          val signature = surface.types(entry.typeIndex.toInt)
          val _         = functions.put(
            entry.name,
            new ProxyExecutable:
              def execute(values: Value*): Object =
                val args = signature.params.zip(values).map((kind, value) =>
                  if kind == 0x7e then value.asLong() else value.asInt().toLong
                )
                val result = kernel.invoke(
                  namespace,
                  entry.name,
                  args,
                  new kernel.GuestMemory:
                    private def memory = active.getOrElse(
                      throw IllegalStateException("guest memory unavailable during start")
                    ).getMember("memory")
                    def read(offset: Int, length: Int): Array[Byte] =
                      require(
                        offset >= 0 && length >= 0 && offset.toLong + length <= memory.getBufferSize(),
                        "guest memory read out of bounds"
                      )
                      val bytes = new Array[Byte](length)
                      memory.readBuffer(offset.toLong, bytes, 0, length)
                      bytes
                    def write(offset: Int, bytes: Array[Byte]): Unit =
                      require(
                        offset >= 0 && offset.toLong + bytes.length <= memory.getBufferSize(),
                        "guest memory write out of bounds"
                      )
                      bytes.zipWithIndex.foreach((byte, index) => memory.writeBufferByte(offset.toLong + index, byte))
                )
                if signature.results.isEmpty then null
                else if signature.results.head == 0x7e then java.lang.Long.valueOf(result(0))
                else Integer.valueOf(result(0).toInt)
          )
        }
        val _ = imports.put(namespace, ProxyObject.fromMap(functions))
      }
      val module  = context.eval(Source.newBuilder("wasm", ByteSequence.create(bytes), "guest").build())
      val exports = module.newInstance(ProxyObject.fromMap(imports)).getMember("exports")
      active = Present(exports)
      if surface.exports.contains("_initialize") then
        val _ = exports.getMember("_initialize").execute()
      new GuestSession:
        override def abi                           = "extism-kernel-v1"
        def answer                                 = exports.getMember("answer").execute().asInt()
        def handle(input: Array[Byte]): GuestReply =
          kernel.begin(input)
          kernel.finish(exports.getMember("handle").execute().asInt())
        def close(): Unit = context.close()
    catch
      case error: Throwable =>
        context.close(true)
        throw error
