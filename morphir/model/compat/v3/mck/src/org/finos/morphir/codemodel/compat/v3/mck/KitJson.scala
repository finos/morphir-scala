package org.finos.morphir.codemodel.compat.v3.mck

import kyo.*

/**
 * The Morphir Compatibility Kit's canonical JSON text. The runner compares an adapter's canonical output with the kit's
 * as text, line by line, so the spacing is part of the contract: a non-empty object is padded inside its braces, and
 * members and elements are separated by `, `. Empty containers are `{}` and `[]`. Everything is on one line. Strings
 * escape what JSON requires and nothing more.
 */
private[mck] object KitJson:

  def write(value: Structure.Value): String =
    val out = new StringBuilder
    append(out, value)
    out.toString

  private def append(out: StringBuilder, value: Structure.Value): Unit =
    value match
      case Structure.Value.Record(fields)     => appendObject(out, fields)
      case Structure.Value.MapEntries(fields) => appendObject(out, fields.map((key, member) => (keyText(key), member)))
      case Structure.Value.VariantCase(name, member) => appendObject(out, Chunk(name -> member))
      case Structure.Value.Sequence(elements)        =>
        out.append('[')
        appendSeparated(out, elements)(append)
        out.append(']')
      case Structure.Value.Str(text)       => appendString(out, text)
      case Structure.Value.Bool(flag)      => out.append(if flag then "true" else "false")
      case Structure.Value.Integer(number) => out.append(number)
      case Structure.Value.Decimal(number) => out.append(number.toString)
      case Structure.Value.BigNum(number)  => out.append(number.bigDecimal.toString)
      case Structure.Value.Null            => out.append("null")
      case Structure.Value.Bytes(bytes) => appendString(out, java.util.Base64.getEncoder.encodeToString(bytes.toArray))
      case Structure.Value.Instant(instant)   => appendString(out, instant.toString)
      case Structure.Value.Duration(duration) => appendString(out, duration.toString)

  private def appendObject(out: StringBuilder, fields: Chunk[(String, Structure.Value)]): Unit =
    if fields.isEmpty then out.append("{}")
    else
      out.append("{ ")
      appendSeparated(out, fields) { (target, field) =>
        appendString(target, field._1)
        target.append(": ")
        append(target, field._2)
      }
      out.append(" }")

  private def appendSeparated[A](out: StringBuilder, items: Chunk[A])(appendItem: (StringBuilder, A) => Unit): Unit =
    items.zipWithIndex.foreach { (item, index) =>
      if index > 0 then out.append(", ")
      appendItem(out, item)
    }

  private def keyText(key: Structure.Value): String =
    key match
      case Structure.Value.Str(text) => text
      case other                     => write(other)

  private def appendString(out: StringBuilder, text: String): Unit =
    out.append('"')
    text.foreach {
      case '"'                      => out.append("\\\"")
      case '\\'                     => out.append("\\\\")
      case '\b'                     => out.append("\\b")
      case '\f'                     => out.append("\\f")
      case '\n'                     => out.append("\\n")
      case '\r'                     => out.append("\\r")
      case '\t'                     => out.append("\\t")
      case control if control < ' ' => out.append(f"\\u${control.toInt}%04x")
      case other                    => out.append(other)
    }
    out.append('"')
end KitJson
