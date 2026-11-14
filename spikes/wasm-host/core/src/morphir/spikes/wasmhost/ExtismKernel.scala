package morphir.spikes.wasmhost

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets.UTF_8

/** Bounded compatibility subset for the pinned Morphir Extism guests, not a general Extism implementation. */
private[wasmhost] final class ExtismKernel(outputLimit: Int):
  trait GuestMemory:
    def read(offset: Int, length: Int): Array[Byte]
    def write(offset: Int, bytes: Array[Byte]): Unit

  private var input           = Array.emptyByteArray
  private var output          = Array.emptyByteArray
  private var error           = ""
  private var next            = 8L
  private val blocks          = scala.collection.mutable.TreeMap.empty[Long, Array[Byte]]
  private val variables       = scala.collection.mutable.Map.empty[String, Array[Byte]]
  private val cache           = scala.collection.mutable.Map.empty[String, Array[Byte]]
  private var diagnosticBytes = 0

  private val signatures = Map(
    "input_length"   -> (Vector.empty[Int], Vector(0x7e)),
    "input_load_u8"  -> (Vector(0x7e), Vector(0x7f)),
    "input_load_u64" -> (Vector(0x7e), Vector(0x7e)),
    "alloc"          -> (Vector(0x7e), Vector(0x7e)),
    "length"         -> (Vector(0x7e), Vector(0x7e)),
    "free"           -> (Vector(0x7e), Vector.empty[Int]),
    "load_u8"        -> (Vector(0x7e), Vector(0x7f)),
    "load_u64"       -> (Vector(0x7e), Vector(0x7e)),
    "store_u8"       -> (Vector(0x7e, 0x7f), Vector.empty[Int]),
    "store_u64"      -> (Vector(0x7e, 0x7e), Vector.empty[Int]),
    "output_set"     -> (Vector(0x7e, 0x7e), Vector.empty[Int]),
    "error_set"      -> (Vector(0x7e), Vector.empty[Int]),
    "config_get"     -> (Vector(0x7e), Vector(0x7e)),
    "var_get"        -> (Vector(0x7e), Vector(0x7e)),
    "var_set"        -> (Vector(0x7e, 0x7e), Vector.empty[Int]),
    "get_log_level"  -> (Vector.empty[Int], Vector(0x7f)),
    "log_info"       -> (Vector(0x7e), Vector.empty[Int]),
    "log_warn"       -> (Vector(0x7e), Vector.empty[Int]),
    "log_error"      -> (Vector(0x7e), Vector.empty[Int]),
    "log_debug"      -> (Vector(0x7e), Vector.empty[Int]),
    "log_trace"      -> (Vector(0x7e), Vector.empty[Int])
  )
  private val services = Map(
    "increment"                  -> (Vector(0x7e), Vector(0x7e)),
    "morphir_get_workspace_info" -> (Vector.empty[Int], Vector(0x7e)),
    "morphir_cache_ir"           -> (Vector(0x7e, 0x7e), Vector.empty[Int]),
    "morphir_get_cached_ir"      -> (Vector(0x7e), Vector(0x7e)),
    "morphir_log"                -> (Vector(0x7e, 0x7e), Vector.empty[Int])
  )
  private val wasi = Map(
    "args_sizes_get"    -> Vector(0x7f, 0x7f),
    "args_get"          -> Vector(0x7f, 0x7f),
    "environ_sizes_get" -> Vector(0x7f, 0x7f),
    "environ_get"       -> Vector(0x7f, 0x7f),
    "clock_time_get"    -> Vector(0x7f, 0x7e, 0x7f),
    "random_get"        -> Vector(0x7f, 0x7f),
    "fd_write"          -> Vector(0x7f, 0x7f, 0x7f, 0x7f),
    "proc_exit"         -> Vector(0x7f)
  )

  def admit(surface: WasmSurface.Surface): Unit = surface.imports.foreach { entry =>
    require(entry.kind == 0 && entry.typeIndex < surface.types.size, "Extism requires function imports")
    val expected = entry.module match
      case "extism:host/env"        => signatures.get(entry.name)
      case "extism:host/user"       => services.get(entry.name)
      case "wasi_snapshot_preview1" => wasi.get(entry.name).map(params =>
          (params, if entry.name == "proc_exit" then Vector.empty[Int] else Vector(0x7f))
        )
      case _ => None
    require(
      expected.exists((params, results) =>
        surface.types(entry.typeIndex.toInt) == WasmSurface.Function(params, results)
      ),
      s"unsupported Extism import or signature ${entry.module}.${entry.name}"
    )
  }

  def begin(bytes: Array[Byte]): Unit =
    require(bytes.length <= 1048576, "request exceeds 1 MiB")
    input = bytes
    output = Array.emptyByteArray
    error = ""
    blocks.clear()
    next = 8L
    diagnosticBytes = 0

  def finish(status: Int): GuestReply =
    require(status == 0 && error.isEmpty, s"Extism guest failed: status=$status $error")
    GuestReply(output.length, output.clone())

  private def allocate(bytes: Array[Byte]): Long =
    require(
      bytes.length <= outputLimit && next + bytes.length + 8 <= 256L * 1024 * 1024,
      "Extism allocation bound exceeded"
    )
    val offset = next
    next += math.max(8L, ((bytes.length.toLong + 7) / 8) * 8)
    blocks(offset) = bytes
    offset
  private def block(offset: Long, length: Int): (Array[Byte], Int) =
    require(offset >= 0 && length >= 0, "unsigned Extism pointer/length out of range")
    val allocation = blocks.maxBefore(offset + 1).getOrElse(throw IllegalArgumentException("unknown Extism allocation"))
    val index      = offset - allocation._1
    require(index <= allocation._2.length && length <= allocation._2.length - index, "Extism buffer out of bounds")
    (allocation._2, index.toInt)
  private def read(offset: Long, length: Int): Array[Byte] =
    val (bytes, index) = block(offset, length)
    java.util.Arrays.copyOfRange(bytes, index, index + length)
  private def whole(offset: Long): Array[Byte] =
    blocks.getOrElse(offset, throw IllegalArgumentException("unknown Extism handle"))
  private def text(offset: Long): String = MepWorker.utf8(whole(offset))
  private def log(message: String): Unit =
    diagnosticBytes += message.getBytes(UTF_8).length
    require(diagnosticBytes <= 65536, "guest diagnostic output exceeds 64 KiB per call")
    System.err.println(ujson.Obj("guestDiagnostic" -> message).render())
  private def integer(value: Int): Array[Byte] =
    ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(value).array()

  def invoke(module: String, name: String, args: Seq[Long], memory: => GuestMemory): Array[Long] =
    def result(value: Long) = Array(value)
    val unit                = Array.emptyLongArray
    if module == "extism:host/env" then
      name match
        case "input_length"                     => result(input.length.toLong)
        case "input_load_u8" | "input_load_u64" =>
          val length = if name == "input_load_u8" then 1 else 8
          require(args.head >= 0 && args.head <= input.length - length, "input offset outside request")
          result(if length == 1 then input(args.head.toInt) & 255L
          else ByteBuffer.wrap(input, args.head.toInt, 8).order(ByteOrder.LITTLE_ENDIAN).getLong())
        case "alloc" =>
          require(args.head >= 0 && args.head <= outputLimit, "allocation exceeds bound");
          result(allocate(new Array[Byte](args.head.toInt)))
        case "length"   => result(blocks.get(args.head).fold(0L)(_.length.toLong))
        case "free"     => val _ = blocks.remove(args.head); unit
        case "load_u8"  => result(read(args.head, 1)(0) & 255L)
        case "load_u64" => result(ByteBuffer.wrap(read(args.head, 8)).order(ByteOrder.LITTLE_ENDIAN).getLong())
        case "store_u8" | "store_u64" =>
          val length         = if name == "store_u8" then 1 else 8
          val (bytes, index) = block(args.head, length)
          if length == 1 then bytes(index) = args(1).toByte
          else ByteBuffer.wrap(bytes, index, 8).order(ByteOrder.LITTLE_ENDIAN).putLong(args(1))
          unit
        case "output_set" =>
          require(args(1) >= 0 && args(1) <= outputLimit, "output exceeds bound")
          output = if args(1) == 0 then Array.emptyByteArray else read(args.head, args(1).toInt)
          unit
        case "error_set"  => error = text(args.head); unit
        case "config_get" => result(0)
        case "var_get"    => result(variables.get(text(args.head)).fold(0L)(bytes => allocate(bytes.clone())))
        case "var_set"    =>
          val key = text(args.head)
          if args(1) == 0 then { val _ = variables.remove(key) }
          else
            val bytes = whole(args(1)).clone()
            require(
              variables.values.map(_.length.toLong).sum + bytes.length <= 1048576,
              "variable storage exceeds 1 MiB"
            )
            variables(key) = bytes
          unit
        case "get_log_level"                   => result(2)
        case value if value.startsWith("log_") => log(text(args.head)); unit
        case _                                 => throw IllegalArgumentException(s"unsupported kernel function $name")
    else if module == "extism:host/user" then
      name match
        case "increment"                  => result(Math.addExact(args.head, 1))
        case "morphir_get_workspace_info" =>
          result(allocate("{\"root\":\"/workspace\",\"output_dir\":\"/workspace/out\"}".getBytes(UTF_8)))
        case "morphir_cache_ir" =>
          require(
            cache.size < 64 && cache.values.map(_.length.toLong).sum + whole(args(1)).length <= 1048576,
            "cache exceeds bound"
          )
          cache(text(args.head)) = whole(args(1)).clone()
          unit
        case "morphir_get_cached_ir" => result(cache.get(text(args.head)).fold(0L)(bytes => allocate(bytes.clone())))
        case "morphir_log"           => log(text(args.head) + ": " + text(args(1))); unit
        case _                       => throw IllegalArgumentException(s"unsupported host service $name")
    else
      name match
        case "args_sizes_get" | "environ_sizes_get" =>
          memory.write(Math.toIntExact(args.head), integer(0))
          memory.write(Math.toIntExact(args(1)), integer(0))
          result(0)
        case "args_get" | "environ_get" => result(0)
        case "clock_time_get"           =>
          val nanos = if args.head == 0 then System.currentTimeMillis() * 1000000L else System.nanoTime()
          memory.write(
            Math.toIntExact(args(2)),
            ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN).putLong(nanos).array()
          )
          result(0)
        case "random_get" =>
          require(args(1) >= 0 && args(1) <= 65536, "random request exceeds bound")
          val bytes = new Array[Byte](args(1).toInt)
          java.security.SecureRandom().nextBytes(bytes)
          memory.write(Math.toIntExact(args.head), bytes)
          result(0)
        case "fd_write" =>
          require(args.head == 1 || args.head == 2, "WASI write descriptor denied")
          require(args(2) >= 0 && args(2) <= 1024, "WASI iovec exceeds bound")
          val written = (0 until args(2).toInt).map { index =>
            val vector = ByteBuffer.wrap(memory.read(
              Math.toIntExact(args(1) + index.toLong * 8),
              8
            )).order(ByteOrder.LITTLE_ENDIAN)
            val offset = vector.getInt()
            val length = vector.getInt()
            require(length >= 0 && length <= 65536 - diagnosticBytes, "WASI diagnostic exceeds bound")
            log(MepWorker.utf8(memory.read(offset, length)))
            length
          }.sum
          memory.write(Math.toIntExact(args(3)), integer(written))
          result(0)
        case "proc_exit" => throw IllegalStateException(s"guest proc_exit ${args.head}")
        case _           => throw IllegalArgumentException(s"WASI capability denied: $name")
