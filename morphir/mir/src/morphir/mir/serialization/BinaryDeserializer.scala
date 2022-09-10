package morphir.mir
package serialization

import java.net.URI
import java.nio.ByteBuffer
import java.io.{DataOutputStream, OutputStream}
import java.nio.charset.StandardCharsets
import morphir.mir.serialization.{Tags => T}
import scala.collection.immutable.ListMap

import scala.collection.mutable

final class BinaryDeserializer(buffer: ByteBuffer, bufferName: String):
  import buffer._

  private[this] var lastPosition: Position = Position.NoPosition

  private val (prelude, header, files): (Prelude, Seq[(Global, Int)], Array[URI]) =
    buffer.position(0)
    val prelude = Prelude.readFrom(buffer, bufferName)
    val files   = Array.fill(getInt())(new URI(getUTF8String()))
    val pairs   = getSeq((getGlobal(), getInt()))
    (prelude, pairs, files)

  private val userEncodedMemberNames = prelude.revision >= 9

  final def deserialize(): Seq[Defn] =
    val allDefns = mutable.UnrolledBuffer.empty[Defn]
    header.foreach { case (g, offset) =>
      buffer.position(offset)
      allDefns += getDefn()
    }
    allDefns.toSeq

  private def getSeq[T](getT: => T): Seq[T] =
    (1 to getInt).map(_ => getT).toSeq

  private def getOpt[T](getT: => T): Option[T] =
    if (get == 0) None else Some(getT)

  private def getInts(): Seq[Int] = getSeq(getInt)

  private def getUTF8String(): String =
    new String(getBytes(), StandardCharsets.UTF_8)

  private def getBytes(): Array[Byte] =
    val arr = new Array[Byte](getInt)
    get(arr)
    arr

  private def getBool(): Boolean = get != 0

  private def getAttrs(): Attrs = Attrs.fromSeq(getSeq(getAttr()))
  private def getAttr(): Attr   = ???

  private def getDefns: Seq[Defn] = getSeq(getDefn())
  private def getDefn(): Defn     = ???

  private def getGlobals(): Seq[Global]      = getSeq(getGlobal())
  private def getGlobalOpt(): Option[Global] = getOpt(getGlobal())
  private def getGlobal(): Global = getInt match
    case T.NoneGlobal   => Global.None
    case T.TopGlobal    => Global.Top(getUTF8String())
    case T.MemberGlobal => Global.Member(Global.Top(getUTF8String()), getSpec())

  private def getSpec(): Spec = ???
