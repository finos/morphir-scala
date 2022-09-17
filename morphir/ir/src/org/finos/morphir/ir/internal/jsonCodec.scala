package org.finos.morphir.ir.internal

import org.finos.morphir.ir.internal.Distribution
import org.finos.morphir.formats.json.Json
import org.finos.morphir.formats.json.JsonString
import org.finos.morphir.formats.errors.DecodingError

object jsonCodec:
  opaque type FormatVersion = 1 | 2

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
