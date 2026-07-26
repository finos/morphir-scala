package morphir.langkit.elm.compiler.abi

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

object InvokeJson:

  given JsonValueCodec[SourceRequest]      = JsonCodecMaker.make
  given JsonValueCodec[PrettyQueryRequest] = JsonCodecMaker.make
  given JsonValueCodec[RunQueryRequest]    = JsonCodecMaker.make
  given JsonValueCodec[InvokeSpan]         = JsonCodecMaker.make
  given JsonValueCodec[InvokeError]        = JsonCodecMaker.make
  given JsonValueCodec[InvokeResponse]     = JsonCodecMaker.make

  def decode[A: JsonValueCodec](json: String): A =
    readFromString[A](json)

  def encode[A: JsonValueCodec](value: A): String =
    writeToString(value)
