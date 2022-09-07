package morphir
package mir
package serialization

import java.net.URI
import java.io.{DataOutputStream, OutputStream}
import java.nio.charset.StandardCharsets
import morphir.mir.serialization.{Tags => T}
import scala.collection.immutable.ListMap
import scala.collection.mutable

final class BinarySerializer:
  private[this] val bufferUnderlying = new JumpBackByteArrayOutputStream
  private[this] val buffer = new DataOutputStream(bufferUnderlying)

  private[this] var lastPosition:Position = Position.NoPosition
  private[this] val fileIndexMap = mutable.Map.empty[URI,Int]

  // Methods were renamed in order to not pollute git blame history.
  // Original implementation used ByteBuffers

  import buffer.{
    write => put,
    writeDouble => putDouble,
    writeFloat => putFloat,
    writeInt => putInt,
    writeChar => putChar,
    writeLong => putLong,
    writeShort => putShort
  }
  import bufferUnderlying.currentPosition
  def serialize(defns:Seq[Defn], outputStream: OutputStream):Unit =
    val names = defns.map(_.name)
    val filenames = initFiles(defns)
    val positions = mutable.UnrolledBuffer.empty[Int]

    Prelude.writeTo(
      buffer,
      Prelude(
        Versions.magic,
        Versions.compat,
        Versions.revision
      )
    )

    putSeq(filenames)(putUTF8String)

    putSeq(names){n =>
      putGlobal(n)
      positions += currentPosition()
      putInt(0)
    }

    buffer.flush()
    bufferUnderlying.writeTo(outputStream)
  end serialize

  private def putSeq[T](seq: Seq[T])(putT: T => Unit) =
    putInt(seq.length)
    seq.foreach(putT)
  private def putOpt[T](opt: Option[T])(putT: T => Unit) = opt match
    case None => put(0.toByte)
    case Some(t) => put(1.toByte); putT(t)
  private def putInts(ints: Seq[Int]) = putSeq[Int](ints)(putInt)

  private def putUTF8String(v: String) = putBytes {
    v.getBytes(StandardCharsets.UTF_8)
  }

  private def putBytes(bytes: Array[Byte]) =
    putInt(bytes.length);
    put(bytes)
  private def putBool(v: Boolean) = put((if (v) 1 else 0).toByte)

  private def putGlobals(globals: Seq[Global]): Unit =
    putSeq(globals)(putGlobal)

  private def putGlobalOpt(globalopt: Option[Global]): Unit =
    putOpt(globalopt)(putGlobal)

  private def putGlobal(global:Global):Unit = global match
    case Global.None =>
      putInt(T.NoneGlobal)
    case Global.Top(id) =>
      putInt(T.TopGlobal)
      putUTF8String(id)
    case Global.Member(Global.Top(owner), spec) =>
      putInt(T.MemberGlobal)
      putUTF8String(owner)
      putSpec(spec)
    case _ =>
      util.unreachable
  end putGlobal

  private def putSpec(spec:Spec):Unit = ()

  private def initFiles(defns: Seq[Defn]): Seq[String] = {
    val filesList = mutable.UnrolledBuffer.empty[String]

    def initFile(pos: Position): Unit = {
      val file = pos.source
      if (pos.isDefined)
        fileIndexMap.getOrElseUpdate(
          file, {
            val idx = filesList.size
            filesList += file.toString
            idx
          }
        )
    }
    defns.foreach {
//     case defn @ Defn.Define(_, _, _, insts) =>
//        initFile(defn.pos)
//        insts.foreach(inst => initFile(inst.pos))
      case defn => initFile(defn.pos)
    }
    filesList.toSeq
  }

end BinarySerializer
