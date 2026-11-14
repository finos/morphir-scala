package morphir.spikes.wasmhost

import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}

/** Admission examines imports and memory declarations before any start function can execute. */
private[wasmhost] object WasmSurface:
  final case class Function(params: Vector[Int], results: Vector[Int])
  final case class Import(module: String, name: String, kind: Int, typeIndex: Long)
  final case class Surface(
      types: Vector[Function],
      imports: Vector[Import],
      exports: Vector[String],
      unboundedMemory: Boolean
  )

  private final class Reader(bytes: Array[Byte]):
    private var offset  = 0
    var unboundedMemory = false
    def done            = offset == bytes.length
    def byte(): Int     =
      require(offset < bytes.length, "truncated Wasm")
      val result = bytes(offset) & 255
      offset += 1
      result
    def unsigned(): Long =
      @annotation.tailrec
      def loop(shift: Int, value: Long): Long =
        require(shift <= 28, "Wasm u32 LEB overflow")
        val next = byte()
        require(shift < 28 || (next & 0xf0) == 0, "Wasm u32 LEB overflow")
        val result = value | ((next & 127).toLong << shift)
        if (next & 128) == 0 then result else loop(shift + 7, result)
      loop(0, 0)
    def count(): Int =
      val n = unsigned()
      require(n <= 1000000, "Wasm vector exceeds inspection bound")
      n.toInt
    def take(n: Long): Array[Byte] =
      require(n >= 0 && n <= bytes.length - offset, "truncated Wasm section")
      val result = java.util.Arrays.copyOfRange(bytes, offset, offset + n.toInt)
      offset += n.toInt
      result
    def text(): String =
      StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
        .decode(ByteBuffer.wrap(take(unsigned()))).toString
    def limits(memory: Boolean): Unit =
      val flags = unsigned()
      require(flags == 0 || flags == 1, "shared/memory64 declarations are outside this spike's ABI")
      val minimum = unsigned()
      val maximum = if flags == 1 then unsigned() else minimum
      if memory then
        if flags == 0 then unboundedMemory = true
        require(minimum <= 4096 && maximum <= 4096, "guest memory declaration exceeds 256 MiB")
        // Absent maxima are capped by the runtime; admission alone does not enforce growth.
    def function(): Function =
      require(byte() == 0x60, "non-function Wasm types are outside this spike's current ABI")
      Function(Vector.fill(count())(byte()), Vector.fill(count())(byte()))

  def inspect(bytes: Array[Byte]): Surface =
    val file = Reader(bytes)
    require(java.util.Arrays.equals(file.take(8), Array[Byte](0, 97, 115, 109, 1, 0, 0, 0)), "expected core Wasm v1")
    val types           = Vector.newBuilder[Function]
    val imports         = Vector.newBuilder[Import]
    val exports         = Vector.newBuilder[String]
    var unboundedMemory = false
    @annotation.tailrec
    def loop(): Unit =
      if !file.done then
        val id      = file.byte()
        val section = Reader(file.take(file.unsigned()))
        id match
          case 1 => (0 until section.count()).foreach(_ => types += section.function())
          case 2 => (0 until section.count()).foreach { _ =>
              val module = section.text()
              val name   = section.text()
              val kind   = section.byte()
              val index  = kind match
                case 0 => section.unsigned()
                case 1 => val _ = section.byte(); section.limits(false); -1L
                case 2 => section.limits(true); -1L
                case 3 => val _ = section.byte(); val _ = section.byte(); -1L
                case _ => throw IllegalArgumentException("unsupported import kind")
              imports += Import(module, name, kind, index)
            }
          case 5 => (0 until section.count()).foreach(_ => section.limits(true))
          case 7 => (0 until section.count()).foreach { _ =>
              exports += section.text()
              val _ = section.byte()
              val _ = section.unsigned()
            }
          case _ => ()
        unboundedMemory ||= section.unboundedMemory
        loop()
    loop()
    Surface(types.result(), imports.result(), exports.result(), unboundedMemory)

  def requireBytes(bytes: Array[Byte], requireMemoryMaximum: Boolean = false): Unit =
    val surface = inspect(bytes)
    require(
      !requireMemoryMaximum || !surface.unboundedMemory,
      "this backend requires a declared memory maximum to enforce the 256 MiB bound"
    )
    val i32     = 0x7f
    val allowed = Map(
      "input_len"   -> Function(Vector.empty, Vector(i32)),
      "input_byte"  -> Function(Vector(i32), Vector(i32)),
      "output_byte" -> Function(Vector(i32), Vector.empty)
    )
    surface.imports.foreach { entry =>
      require(
        entry.kind == 0 && entry.module == "morphir_bytes_v1" && allowed.contains(entry.name),
        s"unsupported byte ABI import ${entry.module}.${entry.name}"
      )
      require(
        entry.typeIndex < surface.types.size && surface.types(entry.typeIndex.toInt) == allowed(entry.name),
        s"byte ABI signature mismatch for ${entry.name}"
      )
    }
    require(surface.exports.contains("handle"), "guest does not export handle")
