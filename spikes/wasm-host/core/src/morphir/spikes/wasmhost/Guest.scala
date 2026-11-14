package morphir.spikes.wasmhost

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}
import java.security.MessageDigest

private[wasmhost] final case class GuestReply(status: Int, bytes: Array[Byte])

private[wasmhost] trait GuestSession extends AutoCloseable:
  def abi: String                       = "morphir-bytes-v1"
  def guestMemoryBytes: kyo.Maybe[Long] = kyo.Absent
  def answer: Int
  def handle(input: Array[Byte]): GuestReply
  def cancel(): Boolean = false

private[wasmhost] trait Backend:
  def name: String
  def supportsCancellation: Boolean = false
  def open(path: Path, outputLimit: Int): GuestSession
  def openWithFuel(path: Path, outputLimit: Int, fuel: Long): GuestSession =
    require(fuel == 5000000000L, "custom instruction metering is unavailable for this backend")
    open(path, outputLimit)

/** Callback state belongs to one serial guest session, never to an engine or shared singleton. */
private[wasmhost] final class ByteCallbacks(val outputLimit: Int):
  private var currentInput = Array.emptyByteArray
  private val output       = ByteArrayOutputStream()

  def begin(input: Array[Byte]): Unit =
    currentInput = input
    output.reset()

  def inputLength: Int = currentInput.length

  def inputByte(index: Int): Int =
    require(index >= 0 && index < currentInput.length, "guest input offset outside request")
    currentInput(index) & 0xff

  def outputByte(value: Int): Unit =
    require(value >= 0 && value <= 255, "guest output value outside byte range")
    require(output.size() < outputLimit, "guest output exceeds configured bound")
    output.write(value)

  def finish(status: Int): GuestReply =
    val bytes = output.toByteArray
    require(status >= -2, "unknown negative guest handle status")
    require(status > 0 || bytes.isEmpty, "non-response status emitted response bytes")
    require(status <= 0 || status == bytes.length, "guest response length disagrees with emitted bytes")
    GuestReply(status, bytes)

private[wasmhost] object GuestFiles:
  val MaxBytes = 64 * 1024 * 1024

  def read(path: Path): Array[Byte] =
    val stream = Files.newInputStream(path)
    try
      val bytes = stream.readNBytes(MaxBytes + 1)
      require(bytes.length <= MaxBytes, "guest exceeds 64 MiB source bound")
      bytes
    finally stream.close()

  def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

  def sha256(path: Path): String =
    val digest = MessageDigest.getInstance("SHA-256")
    val stream = java.security.DigestInputStream(Files.newInputStream(path), digest)
    try
      val _ = stream.transferTo(java.io.OutputStream.nullOutputStream())
      digest.digest().map(b => f"${b & 0xff}%02x").mkString
    finally stream.close()
