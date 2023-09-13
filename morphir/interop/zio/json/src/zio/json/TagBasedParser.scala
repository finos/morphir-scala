package zio.json

import org.finos.morphir.ir.Value.Value
import zio.json.JsonDecoder.{JsonError, UnsafeJson}
import zio.json.internal.RetractReader

import scala.util.control.Breaks.{break, breakable}

case class TagBasedParser[T](pf: PartialFunction[String, JsonDecoder[T]]) extends JsonDecoder[T] {
  override def unsafeDecode(trace: List[JsonError], inRaw: RetractReader): T = {
    val in = new zio.json.internal.WithRecordingReader(inRaw, 64)

    def err(msg: String) = throw UnsafeJson(JsonError.Message(msg) :: trace)

    if (in.nextNonWhitespace() != '[') err("Expected first char to be '['")
    if (in.nextNonWhitespace() != '"') err("Expected second char to be quote ('\"')")

    val buff           = new StringBuffer()
    var readChar: Char = 0
    breakable {
      while (true) {
        readChar = in.nextNonWhitespace()
        if (readChar != 0 && (readChar.isLetter || readChar == '_')) {
          // buff.append returns itself (so that you can chain it) but we don't care about that here
          val _ = buff.append(readChar)
        } else {
          break()
        }
      }
    }

    // rewind to the beginning of the clause
    in.rewind()
    val term = buff.toString

    val output: T =
      if (pf.isDefinedAt(term)) pf.apply(term).unsafeDecode(trace, in)
      else err(s"Undefined heading: '${term}'")

    output
  }
}
