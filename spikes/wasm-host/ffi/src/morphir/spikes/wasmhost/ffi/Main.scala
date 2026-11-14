package morphir.spikes.wasmhost.ffi

import java.lang.foreign.*
import java.lang.foreign.ValueLayout.*
import java.lang.invoke.{MethodHandles, MethodType}
import java.nio.file.Path
import morphir.spikes.wasmhost.*
import kyo.*
import scala.jdk.CollectionConverters.*

object Main extends HostCli(ExtismBackend)

private[wasmhost] object ExtismBackend extends Backend:
  def name                                             = "extism-ffm"
  override def supportsCancellation                    = true
  def open(path: Path, outputLimit: Int): GuestSession = openWithFuel(path, outputLimit, 5000000000L)
  override def openWithFuel(path: Path, outputLimit: Int, fuel: Long): GuestSession =
    require(fuel > 0, "fuel must be positive")
    val libraryPath  = System.getenv("MORPHIR_EXTISM_LIBRARY")
    val expectedHash = System.getenv("MORPHIR_EXTISM_SHA256")
    require(libraryPath != null && expectedHash != null, "Set MORPHIR_EXTISM_LIBRARY and MORPHIR_EXTISM_SHA256")
    val library = Path.of(libraryPath).toAbsolutePath
    require(java.nio.file.Files.isRegularFile(library), "native library is missing")
    require(GuestFiles.sha256(library) == expectedHash, "native library digest mismatch")
    require(
      System.getProperty("os.name") == "Mac OS X" && System.getProperty("os.arch") == "aarch64",
      "verified native artifact requires macOS aarch64"
    )
    val header = java.nio.file.Files.newInputStream(library)
    try
      require(
        java.util.Arrays.equals(header.readNBytes(8), Array[Byte](-49, -6, -19, -2, 12, 0, 0, 1)),
        "native library architecture mismatch: expected Mach-O arm64"
      )
    finally header.close()
    val session = ExtismSession(library, outputLimit)
    try
      session.initialize(path, fuel)
      session
    catch
      case error: Throwable =>
        session.close()
        throw error

/** This binding is private to the experiment; no raw pointers cross its Scala host boundary. */
private final class ExtismSession(libraryPath: Path, outputLimit: Int) extends GuestSession:
  private val arena   = Arena.ofShared()
  private val linker  = Linker.nativeLinker()
  private val library = try SymbolLookup.libraryLookup(libraryPath, arena)
  catch
    case error: Throwable =>
      arena.close()
      throw error
  private val lifecycle                         = java.util.concurrent.locks.ReentrantReadWriteLock()
  private val callLock                          = new Object()
  private var callbackFailure: Maybe[Throwable] = Absent
  private var closed                            = false
  private var plugin                            = MemorySegment.NULL
  private var cancelHandle                      = MemorySegment.NULL
  private val functions                         = scala.collection.mutable.ArrayBuffer.empty[MemorySegment]
  private val cache                             = scala.collection.mutable.Map.empty[String, Array[Byte]]
  private val bytesCallback                     = ByteCallbacks(outputLimit)
  private var byteProfile                       = false
  override def abi                              = if byteProfile then "morphir-bytes-v1" else "extism-kernel-v1"

  private def call(name: String, descriptor: FunctionDescriptor, args: AnyRef*): AnyRef =
    val handle = linker.downcallHandle(library.find(name).orElseThrow(), descriptor)
    handle.invokeWithArguments(args.asJava)

  private def pointer(name: String, params: MemoryLayout*)(args: AnyRef*): MemorySegment =
    call(name, FunctionDescriptor.of(ADDRESS, params*), args*).asInstanceOf[MemorySegment]

  private def release(name: String, address: MemorySegment): Unit =
    if address != MemorySegment.NULL then
      val _ = call(name, FunctionDescriptor.ofVoid(ADDRESS), address)

  private val callbackDescriptor = FunctionDescriptor.ofVoid(ADDRESS, ADDRESS, JAVA_LONG, ADDRESS, JAVA_LONG, ADDRESS)
  private val callbackMethod     = MethodHandles.lookup().findVirtual(
    classOf[ExtismSession],
    "dispatch",
    MethodType.methodType(
      java.lang.Void.TYPE,
      classOf[MemorySegment],
      classOf[MemorySegment],
      java.lang.Long.TYPE,
      classOf[MemorySegment],
      java.lang.Long.TYPE,
      classOf[MemorySegment]
    )
  ).bindTo(this)
  private val callback    = linker.upcallStub(callbackMethod, callbackDescriptor, arena)
  private val definitions = Vector(
    ("extism:host/user", "increment", Vector(1), Vector(1)),
    ("extism:host/user", "morphir_get_workspace_info", Vector.empty[Int], Vector(1)),
    ("extism:host/user", "morphir_cache_ir", Vector(1, 1), Vector.empty[Int]),
    ("extism:host/user", "morphir_get_cached_ir", Vector(1), Vector(1)),
    ("extism:host/user", "morphir_log", Vector(1, 1), Vector.empty[Int]),
    ("morphir_bytes_v1", "input_len", Vector.empty[Int], Vector(0)),
    ("morphir_bytes_v1", "input_byte", Vector(0), Vector(0)),
    ("morphir_bytes_v1", "output_byte", Vector(0), Vector.empty[Int])
  )

  def initialize(guest: Path, fuel: Long): Unit =
    val wasmBytes = GuestFiles.read(guest)
    val surface   = WasmSurface.inspect(wasmBytes)
    byteProfile = surface.imports.exists(_.module == "morphir_bytes_v1")
    if byteProfile then WasmSurface.requireBytes(wasmBytes)
    val allowedHost = definitions.map(d => (d._1, d._2)).toSet
    surface.imports.foreach { entry =>
      require(
        entry.kind == 0 &&
          (allowedHost((entry.module, entry.name)) ||
            entry.module == "extism:host/env" || entry.module == "wasi_snapshot_preview1"),
        s"unsupported Extism import ${entry.module}.${entry.name}"
      )
    }
    definitions.zipWithIndex.foreach { case ((namespace, name, inputs, outputs), index) =>
      def types(values: Vector[Int]): MemorySegment =
        val result = arena.allocate(math.max(1, values.size).toLong * 4, 4)
        values.zipWithIndex.foreach((value, i) => result.set(JAVA_INT, i.toLong * 4, value))
        result
      val data     = arena.allocateFrom(JAVA_INT, index)
      val function = pointer(
        "extism_function_new",
        ADDRESS,
        ADDRESS,
        JAVA_LONG,
        ADDRESS,
        JAVA_LONG,
        ADDRESS,
        ADDRESS,
        ADDRESS
      )(
        arena.allocateFrom(name),
        types(inputs),
        Long.box(inputs.size.toLong),
        types(outputs),
        Long.box(outputs.size.toLong),
        callback,
        data,
        MemorySegment.NULL
      )
      require(function != MemorySegment.NULL, "Extism function creation failed")
      functions += function
      val _ = call(
        "extism_function_set_namespace",
        FunctionDescriptor.ofVoid(ADDRESS, ADDRESS),
        function,
        arena.allocateFrom(namespace)
      )
    }
    val pointers = arena.allocate(ADDRESS, functions.size.toLong)
    functions.zipWithIndex.foreach((function, index) => pointers.setAtIndex(ADDRESS, index.toLong, function))
    val manifest = ujson.Obj(
      "wasm"          -> ujson.Arr(ujson.Obj("data" -> java.util.Base64.getEncoder.encodeToString(wasmBytes))),
      "memory"        -> ujson.Obj("max_pages" -> 4096, "max_var_bytes" -> 1048576),
      "timeout_ms"    -> 30000,
      "allowed_hosts" -> ujson.Arr(),
      "allowed_paths" -> ujson.Obj()
    ).render().getBytes(java.nio.charset.StandardCharsets.UTF_8)
    val temporary = Arena.ofConfined()
    try
      val errorSlot = temporary.allocateFrom(ADDRESS, MemorySegment.NULL)
      plugin = pointer(
        "extism_plugin_new_with_fuel_limit",
        ADDRESS,
        JAVA_LONG,
        ADDRESS,
        JAVA_LONG,
        JAVA_BOOLEAN,
        JAVA_LONG,
        ADDRESS
      )(
        temporary.allocateFrom(JAVA_BYTE, manifest*),
        Long.box(manifest.length.toLong),
        pointers,
        Long.box(functions.size.toLong),
        Boolean.box(true),
        Long.box(fuel),
        errorSlot
      )
      if plugin == MemorySegment.NULL then
        val error = errorSlot.get(ADDRESS, 0)
        try throw IllegalStateException(readMessage(error))
        finally release("extism_plugin_new_error_free", error)
      cancelHandle = pointer("extism_plugin_cancel_handle", ADDRESS)(plugin)
    finally temporary.close()

  /** ExtismVal has an enum at offset 0 and an 8-byte union at offset 8 on the selected target. */
  def dispatch(
      current: MemorySegment,
      inputs: MemorySegment,
      inputCount: Long,
      outputs: MemorySegment,
      outputCount: Long,
      userData: MemorySegment
  ): Unit =
    try
      val index      = userData.reinterpret(4).get(JAVA_INT, 0)
      val definition = definitions(index)
      require(inputCount == definition._3.size && outputCount == definition._4.size, "unexpected Extism callback arity")
      def input(index: Int): Long =
        val value = inputs.reinterpret(inputCount * 16).asSlice(index.toLong * 16, 16)
        val tag   = definition._3(index)
        require(value.get(JAVA_INT, 0) == tag, "unexpected Extism callback value type")
        if tag == 0 then value.get(JAVA_INT, 8).toLong else value.get(JAVA_LONG, 8)
      def output(value: Long): Unit =
        val out = outputs.reinterpret(16)
        val tag = definition._4.head
        out.set(JAVA_INT, 0, tag)
        if tag == 0 then out.set(JAVA_INT, 8, Math.toIntExact(value)) else out.set(JAVA_LONG, 8, value)
      def read(offset: Long): Array[Byte] =
        val length = call(
          "extism_current_plugin_memory_length",
          FunctionDescriptor.of(JAVA_LONG, ADDRESS, JAVA_LONG),
          current,
          Long.box(offset)
        ).asInstanceOf[java.lang.Long].longValue()
        require(offset >= 0 && length >= 0 && length <= outputLimit, "host callback buffer exceeds bound")
        if length == 0 then Array.emptyByteArray
        else
          pointer("extism_current_plugin_memory", ADDRESS)(current).reinterpret(Math.addExact(offset, length))
            .asSlice(offset, length).toArray(JAVA_BYTE)
      def write(bytes: Array[Byte]): Long =
        require(bytes.length <= outputLimit, "host callback result exceeds bound")
        val offset = call(
          "extism_current_plugin_memory_alloc",
          FunctionDescriptor.of(JAVA_LONG, ADDRESS, JAVA_LONG),
          current,
          Long.box(bytes.length.toLong)
        ).asInstanceOf[java.lang.Long].longValue()
        require(offset > 0, "guest allocation failed")
        pointer("extism_current_plugin_memory", ADDRESS)(current).reinterpret(offset + bytes.length)
          .asSlice(offset, bytes.length).copyFrom(MemorySegment.ofArray(bytes))
        offset
      index match
        case 0 => output(Math.addExact(input(0), 1))
        case 1 => output(write("{\"root\":\"/workspace\",\"output_dir\":\"/workspace/out\"}".getBytes(
            java.nio.charset.StandardCharsets.UTF_8
          )))
        case 2 =>
          require(cache.size < 64, "session cache is full")
          cache(MepWorker.utf8(read(input(0)))) = read(input(1))
        case 3 => output(cache.get(MepWorker.utf8(read(input(0)))).fold(0L)(write))
        case 4 =>
          val level   = MepWorker.utf8(read(input(0)))
          val message = MepWorker.utf8(read(input(1)))
          require(level.length + message.length <= 4096, "diagnostic exceeds bound")
          System.err.println(ujson.Obj("hostLogLevel" -> level, "message" -> message).render())
        case 5 => output(bytesCallback.inputLength.toLong)
        case 6 => output(bytesCallback.inputByte(Math.toIntExact(input(0))).toLong)
        case 7 => bytesCallback.outputByte(Math.toIntExact(input(0)))
        case _ => throw IllegalStateException("unknown host function")
    catch case error: Throwable => callbackFailure = Present(error)

  private def readMessage(pointer: MemorySegment): String =
    if pointer == MemorySegment.NULL then "Extism call failed without an error message"
    else pointer.reinterpret(65536).getString(0)

  private def invoke(name: String, input: Array[Byte]): Int =
    require(!closed, "session is closed")
    val temporary = Arena.ofConfined()
    val status    = try
      val buffer = temporary.allocateFrom(JAVA_BYTE, input*)
      call(
        "extism_plugin_call",
        FunctionDescriptor.of(JAVA_INT, ADDRESS, ADDRESS, ADDRESS, JAVA_LONG),
        plugin,
        temporary.allocateFrom(name),
        buffer,
        Long.box(input.length.toLong)
      ).asInstanceOf[Integer].intValue()
    finally temporary.close()
    callbackFailure match
      case Present(error) => throw IllegalStateException("host callback failed", error)
      case Absent         => ()
    status

  private def serial[A](body: => A): A = callLock.synchronized {
    lifecycle.readLock().lock()
    try body
    finally lifecycle.readLock().unlock()
  }

  def answer: Int = serial(invoke("answer", Array.emptyByteArray))

  def handle(input: Array[Byte]): GuestReply =
    serial(handleLocked(input))

  private def handleLocked(input: Array[Byte]): GuestReply =
    require(input.length <= 1048576, "request exceeds 1 MiB")
    if byteProfile then bytesCallback.begin(input)
    val status = invoke("handle", input)
    if byteProfile then return bytesCallback.finish(status)
    if status != 0 then
      throw IllegalStateException(readMessage(pointer("extism_plugin_error", ADDRESS)(plugin)))
    val length = call("extism_plugin_output_length", FunctionDescriptor.of(JAVA_LONG, ADDRESS), plugin)
      .asInstanceOf[java.lang.Long].longValue()
    require(length >= 0 && length <= outputLimit, "Extism output exceeds configured bound")
    val output = pointer("extism_plugin_output_data", ADDRESS)(plugin)
    require(length == 0 || output != MemorySegment.NULL, "null Extism output")
    val bytes = if length == 0 then Array.emptyByteArray else output.reinterpret(length).toArray(JAVA_BYTE)
    GuestReply(bytes.length, bytes)

  override def cancel(): Boolean =
    lifecycle.readLock().lock()
    try
      require(!closed, "session is closed")
      call("extism_plugin_cancel", FunctionDescriptor.of(JAVA_BOOLEAN, ADDRESS), cancelHandle)
        .asInstanceOf[java.lang.Boolean].booleanValue()
    finally lifecycle.readLock().unlock()

  def close(): Unit =
    lifecycle.writeLock().lock()
    try
      if !closed then
        closed = true
        try release("extism_plugin_free", plugin)
        finally
          try functions.foreach(function => release("extism_function_free", function))
          finally arena.close()
    finally lifecycle.writeLock().unlock()
