package org.finos.morphir.ir
package json

import zio._
import zio.json._
import zio.json.ast.Json
import org.finos.morphir.ir.distribution.Distribution
import scala.annotation.nowarn

trait MorphirJsonFileSupport extends JsonEncodingHelpers {
  implicit val morphirIRVersionDecoder: JsonDecoder[MorphirIRVersion] = JsonDecoder.int.map {
    case 1 => MorphirIRVersion.V1_0
    case 2 => MorphirIRVersion.V2_0
    case 3 => MorphirIRVersion.V3_0
  }

  private def DecodeDistributionWithVersion(version: MorphirIRVersion, distribution: Json) = version match {
    case MorphirIRVersion.V1_0 =>
      import org.finos.morphir.ir.json.MorphirJsonDecodingSupportV1._
      JsonDecoder[Distribution].fromJsonAST(distribution)
    case MorphirIRVersion.V2_0 =>
      import org.finos.morphir.ir.json.MorphirJsonDecodingSupportV2._
      JsonDecoder[Distribution].fromJsonAST(distribution)
    case MorphirIRVersion.V3_0 =>
      import org.finos.morphir.ir.json.MorphirJsonDecodingSupport._
      JsonDecoder[Distribution].fromJsonAST(distribution)
  }

  implicit def morphirIRFileDecoder: JsonDecoder[MorphirIRFile] =
    Json.Obj.decoder.mapOrFail[MorphirIRFile] {
      case Json.Obj(
            Chunk(
              "formatVersion" -> formatVersion,
              "distribution" -> distribution
            )
          ) =>
        for {
          version <- JsonDecoder[MorphirIRVersion].fromJsonAST(formatVersion)
          dist    <- DecodeDistributionWithVersion(version, distribution)
        } yield MorphirIRFile(version, dist)

      case other => Left(s"Cannot decode: $other")
    }

  implicit val morphirIRVersionEncoder: JsonEncoder[MorphirIRVersion] =
    JsonEncoder.int.contramap(_.versionNumber.toDouble.toInt)

  implicit def morphirIRFileJsonEncoder: JsonEncoder[MorphirIRFile] =
    Json.encoder.contramap[MorphirIRFile] { file =>
      Json.Obj(
        "formatVersion" -> toJsonAstOrThrow(file.version),
        "distribution"  -> EncodeDistributionWithVersion(file.version, file.distribution)
      )
    }

  private def EncodeDistributionWithVersion(version: MorphirIRVersion, distribution: Distribution) = version match {
    case MorphirIRVersion.V1_0 =>
      import org.finos.morphir.ir.json.MorphirJsonEncodingSupportV1._
      super.toJsonAstOrThrow(distribution)
    case MorphirIRVersion.V2_0 =>
      import org.finos.morphir.ir.json.MorphirJsonEncodingSupportV2._
      super.toJsonAstOrThrow(distribution)
    case MorphirIRVersion.V3_0 =>
      import org.finos.morphir.ir.json.MorphirJsonEncodingSupport._
      super.toJsonAstOrThrow(distribution)
  }
}

object MorphirJsonFileSupport extends MorphirJsonFileSupport
