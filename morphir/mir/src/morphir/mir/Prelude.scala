package morphir.mir
import java.nio.ByteBuffer
import java.io.DataOutputStream

case class Prelude(magic: Int, compat: Int, revision: Int)
object Prelude:
  val length = 12

  def readFrom(buffer: ByteBuffer, bufferName: String): Prelude =
    val magic    = buffer.getInt
    val compat   = buffer.getInt
    val revision = buffer.getInt
    assert(magic == Versions.magic, "Can't read non-MIR file")
    assert(
      compat == Versions.compat && revision <= Versions.revision,
      "Can't read binary-incompatible version of MIR from '" + bufferName +
        "' (expected compat=" + Versions.compat + ", got " + compat +
        "; expected revision=" + Versions.revision + ", got " + revision + ")."
    )
    Prelude(magic, compat, revision)
  end readFrom

  def writeTo(out: DataOutputStream, prelude: Prelude): DataOutputStream =
    out.writeInt(prelude.magic)
    out.writeInt(prelude.compat)
    out.writeInt(prelude.revision)
    out
  end writeTo
