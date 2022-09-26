package org.finos.morphir.ir.internal

import org.finos.morphir.ir.internal.Distribution
import org.finos.morphir.formats.json.Json
import org.finos.morphir.formats.json.JsonString
import org.finos.morphir.formats.errors.DecodingError

object jsonCodec:
  opaque type FormatVersion = 1 | 2
  object FormatVersion:
    final val V1: FormatVersion      = 1
    final val V2: FormatVersion      = 2
    final val Default: FormatVersion = V2

    def valueOf(input: FormatVersion): Int = input

  extension (input: FormatVersion) def value: Int = input

  trait DistributionEncoders(using currentFormatVersion: FormatVersion):
    def encodeVersionedDistribution(distro: Distribution): Json
    def encodeDistribution(distro: Distribution): Json
  end DistributionEncoders

  trait DistributionDecoders(using currentFormatVersion: FormatVersion):
    def decodeVersionedDistribution(json: Json): Either[DecodingError, Distribution]
    def decodeDistribution(json: Json): Either[DecodingError, Distribution]
  end DistributionDecoders

  trait DistributionCodec(using currentFormatVersion: FormatVersion)
      extends DistributionEncoders
      with DistributionDecoders

  trait MorphirEncoders(using currentFormatVersion: FormatVersion):
    def encodeUnit(scala: Unit): Json
  end MorphirEncoders
