package org.finos.morphir.datamodel.codecs

import io.bullet.borer._
import io.bullet.borer.derivation.MapBasedCodecs._
import org.finos.morphir._
import org.finos.morphir.naming._
import org.finos.morphir.datamodel._
import org.finos.morphir.datamodel.codecs.BaseCodecs._
import org.finos.morphir.datamodel.codecs.NamingCodecs._
import scala.collection.mutable

object MdmCodecs {
  implicit val labelCodec: Codec[Label]                = deriveCodec[Label]
  implicit val enumLabelCodec: Codec[EnumLabel]        = deriveAllCodecs[EnumLabel]
  implicit val enumCaseCodec: Codec[Concept.Enum.Case] = deriveCodec[Concept.Enum.Case]
  implicit val enumCodec: Codec[Concept.Enum]          = deriveCodec[Concept.Enum]
  implicit val conceptCodec: Codec[Concept]            = deriveAllCodecs[Concept]
  // deriveAllCodecs is supposed to derive for all branches of an ADT, and it does, but when the leaves are referenced
  // in another ADT (such as Concept in Data), explicit derivation is necessary.
  implicit val aliasCodec: Codec[Concept.Alias]       = deriveCodec[Concept.Alias]
  implicit val listCodec: Codec[Concept.List]         = deriveCodec[Concept.List]
  implicit val mapCodec: Codec[Concept.Map]           = deriveCodec[Concept.Map]
  implicit val setCodec: Codec[Concept.Set]           = deriveCodec[Concept.Set]
  implicit val tupleCodec: Codec[Concept.Tuple]       = deriveCodec[Concept.Tuple]
  implicit val unionCodec: Codec[Concept.Union]       = deriveCodec[Concept.Union]
  implicit val recordCodec: Codec[Concept.Record]     = deriveCodec[Concept.Record]
  implicit val resultCodec: Codec[Concept.Result]     = deriveCodec[Concept.Result]
  implicit val optionalCodec: Codec[Concept.Optional] = deriveCodec[Concept.Optional]
  // Derivers for ADT leaves must be explicity declared here (rather than using deriveAllCodecs) because the ADT
  // nests types under a object
  implicit val dataOptionalSomeCodec: Codec[Data.Optional.Some] = deriveCodec[Data.Optional.Some]
  implicit val dataOptionalNoneCodec: Codec[Data.Optional.None] = deriveCodec[Data.Optional.None]
  implicit val dataResultOkCodec: Codec[Data.Result.Ok]         = deriveCodec[Data.Result.Ok]
  implicit val dataResultErrCodec: Codec[Data.Result.Err]       = deriveCodec[Data.Result.Err]
  implicit val dataTupleCodec: Codec[Data.Tuple]                = deriveCodec[Data.Tuple]
  implicit val dataRecordCodec: Codec[Data.Record]              = deriveCodec[Data.Record]
  implicit val dataOptionalCodec: Codec[Data.Optional]          = deriveCodec[Data.Optional]
  implicit val dataListCodec: Codec[Data.List]                  = deriveCodec[Data.List]
  implicit val dataSetCodec: Codec[Data.Set]                    = deriveCodec[Data.Set]
  implicit val dataMapCodec: Codec[Data.Map]                    = deriveCodec[Data.Map]
  implicit val dataAliasedCodec: Codec[Data.Aliased]            = deriveCodec[Data.Aliased]
  implicit val dataUnionCodec: Codec[Data.Union]                = deriveCodec[Data.Union]
  implicit val dataStructCodec: Codec[Data.Struct]              = deriveCodec[Data.Struct]
  implicit val dataCodec: Codec[Data]                           = deriveAllCodecs[Data]

  implicit val linkedHashMapEncoder: Encoder[mutable.LinkedHashMap[Data, Data]] =
    Encoder { (writer, map: mutable.LinkedHashMap[Data, Data]) =>
      writer.writeInt(map.size)
      map.keySet.foreach { key =>
        val value = map(key)
        writer.write(Cbor.encode(key).toByteArray)
        writer.write(Cbor.encode(value).toByteArray)
      }
      writer
    }

  implicit val linkedHashMapDecoder: Decoder[mutable.LinkedHashMap[Data, Data]] = Decoder { reader =>
    val count = reader.readInt()
    val map   = new mutable.LinkedHashMap[Data, Data]()
    val tuples = (0 until count).map { _ =>
      val key   = Cbor.decode(reader.readByteArray()).to[Data].value
      val value = Cbor.decode(reader.readByteArray()).to[Data].value
      map += key -> value
    }
    map
  }
}
