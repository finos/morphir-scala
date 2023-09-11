package org.finos.morphir.codec

import zio.Chunk
import zio.stream.ZPipeline

trait BinaryCodec[A] extends Codec[Chunk[Byte], Byte, A]

object BinaryCodec {

  type BinaryEncoder[A] = Encoder[Chunk[Byte], Byte, A]

  type BinaryDecoder[A] = Decoder[Chunk[Byte], Byte, A]

  type BinaryStreamEncoder[A] = ZPipeline[Any, Nothing, A, Byte]

  type BinaryStreamDecoder[A] = ZPipeline[Any, DecodeError, Byte, A]

}

