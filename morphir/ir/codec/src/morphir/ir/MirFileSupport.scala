package morphir.ir
import io.bullet.borer.{Cbor, Codec, Decoder,  Encoder, Json}
import io.bullet.borer.derivation.key
import io.bullet.borer.derivation.ArrayBasedCodecs._

trait MirFileSupport:
  given Encoder[Name] = Encoder.forArray[String].contramap[Name](_.toList.toArray)
  given Decoder[Name] = Decoder.forArray[String].map[Name](Name.wrap(_))

object MirFileSupport extends MirFileSupport
