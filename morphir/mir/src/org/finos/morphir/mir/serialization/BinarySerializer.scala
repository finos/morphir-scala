package org.finos
package morphir
package mir
package serialization

import java.net.URI
import java.io.{DataOutputStream, OutputStream}
import java.nio.charset.StandardCharsets
import morphir.mir.serialization.{Tags => T}
import org.finos.morphir.util.unreachable
import scala.collection.immutable.ListMap
import scala.collection.mutable

final class BinarySerializer:
  private[this] val bufferUnderlying = new JumpBackByteArrayOutputStream
  private[this] val buffer           = new DataOutputStream(bufferUnderlying)

  private[this] var lastPosition: Position = Position.NoPosition
  private[this] val fileIndexMap           = mutable.Map.empty[URI, Int]

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
  def serialize(defns: Seq[Defn], outputStream: OutputStream): Unit =
    val names     = defns.map(_.name)
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

    putSeq(names) { n =>
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
    case None    => put(0.toByte)
    case Some(t) => put(1.toByte); putT(t)
  private def putInts(ints: Seq[Int]) = putSeq[Int](ints)(putInt)

  private def putUTF8String(v: String) = putBytes {
    v.getBytes(StandardCharsets.UTF_8)
  }

  private def putBytes(bytes: Array[Byte]) =
    putInt(bytes.length)
    put(bytes)
  private def putBool(v: Boolean) = put((if (v) 1 else 0).toByte)

  private def putAttrs(attr: Attrs): Unit = putSeq(attr.toSeq)(putAttr)
  private def putAttr(attr: Attr): Unit   = ()

  private def putGlobals(globals: Seq[Global]): Unit =
    putSeq(globals)(putGlobal)

  private def putGlobalOpt(globalopt: Option[Global]): Unit =
    putOpt(globalopt)(putGlobal)

  private def putGlobal(global: Global): Unit = global match
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
      unreachable
  end putGlobal

  private def putSpec(spec: Spec): Unit = ()

  // Ported from Scala.js
  def putPosition(pos: Position): Unit =
    import PositionFormat._
    def writeFull(): Unit =
      put(FormatFullMaskValue.toByte)
      putInt(fileIndexMap(pos.source))
      putInt(pos.line)
      putInt(pos.column)
    end writeFull

    if (pos == Position.NoPosition) put(FormatNoPositionValue.toByte)
    else if (
      lastPosition == Position.NoPosition ||
      pos.source != lastPosition.source
    )
      writeFull()
      lastPosition = pos
    else
      val line         = pos.line
      val column       = pos.column
      val lineDiff     = line - lastPosition.line
      val columnDiff   = column - lastPosition.column
      val columnIsByte = column >= 0 && column < 256

      if (lineDiff == 0 && columnDiff >= -64 && columnDiff < 64)
        put(((columnDiff << Format1Shift) | Format1MaskValue).toByte)
      else if (lineDiff >= -32 && lineDiff < 32 && columnIsByte)
        put(((lineDiff << Format2Shift) | Format2MaskValue).toByte)
        put(column.toByte)
      else if (lineDiff >= Short.MinValue && lineDiff <= Short.MaxValue && columnIsByte)
        put(Format3MaskValue.toByte)
        putShort(lineDiff.toShort)
        put(column.toByte)
      else writeFull()
      end if

      lastPosition = pos
    end if
  end putPosition

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
