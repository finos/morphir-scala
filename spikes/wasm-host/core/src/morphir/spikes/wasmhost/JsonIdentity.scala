package morphir.spikes.wasmhost

import kyo.*

/** Reads a top-level JSON-RPC identity without converting uint64 request ids through Double. */
private[wasmhost] object JsonIdentity:
  def extract(json: String): Maybe[String] =
    @annotation.tailrec
    def whitespace(i: Int): Int =
      if i < json.length && json(i).isWhitespace then whitespace(i + 1) else i
    @annotation.tailrec
    def stringEnd(i: Int): Int =
      require(i < json.length, "unterminated JSON string")
      json(i) match
        case '\\' => stringEnd(i + 2)
        case '"'  => i + 1
        case _    => stringEnd(i + 1)
    @annotation.tailrec
    def primitiveEnd(i: Int): Int =
      if i == json.length || json(i) == ',' || json(i) == '}' || json(i).isWhitespace then i
      else primitiveEnd(i + 1)
    @annotation.tailrec
    def loop(i: Int, depth: Int): Maybe[String] =
      if i >= json.length then Absent
      else
        json(i) match
          case '{' | '[' => loop(i + 1, depth + 1)
          case '}' | ']' => loop(i + 1, depth - 1)
          case '"'       =>
            val end   = stringEnd(i + 1)
            val after = whitespace(end)
            if depth == 1 && after < json.length && json(after) == ':' && ujson.read(json.substring(i, end)).str == "id"
            then
              val start   = whitespace(after + 1)
              val stop    = if json(start) == '"' then stringEnd(start + 1) else primitiveEnd(start)
              val literal = json.substring(start, stop)
              if literal == "null" then Present("null")
              else if literal.startsWith("\"") then Present("string:" + ujson.read(literal).str)
              else Present("number:" + new java.math.BigDecimal(literal).stripTrailingZeros().toString)
            else loop(end, depth)
          case _ => loop(i + 1, depth)
    loop(0, 0)
