package morphir.mir
import io.bullet.borer.{Cbor, Codec, Decoder, Encoder, Json}
import io.bullet.borer.derivation.key
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import morphir.mir.file.format.{MirFile, MirFileFormatVersion, MirFileHeader}
import morphir.mir.module.ModuleName

trait MirFileSupport:
  given Encoder[Name] = Encoder.forArray[String].contramap[Name](_.toList.toArray)
  given Decoder[Name] = Decoder.forArray[String].map[Name](Name.wrap(_))

  given Encoder[Path] = Encoder.forArray[Name].contramap(_.toList.toArray)
  given Decoder[Path] = Decoder.forArray[Name].map(Path.wrap(_))

  given Codec[ModuleName]           = deriveCodec[ModuleName]
  given Codec[MirFileFormatVersion] = deriveCodec[MirFileFormatVersion]

  given Codec[MirFileHeader] = deriveCodec[MirFileHeader]
  given Codec[MirFile]       = deriveCodec[MirFile]

object MirFileSupport extends MirFileSupport
