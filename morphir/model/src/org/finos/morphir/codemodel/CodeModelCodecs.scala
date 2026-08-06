package org.finos.morphir.codemodel

import kyo.{Result, Schema}
import kyo.Json.given_Json

/**
 * JSON codecs for the code model, derived from its `Schema` rather than hand-written.
 *
 * This replaces `MorphirJsonEncodingSupportV4`/`DecodingSupportV4` (808 hand-written lines). The v4 wire format is new
 * and unshipped, so the derived layout is authoritative.
 */
object CodeModelCodecs:
  private val distributionSchema = summon[Schema[Distribution]]

  def encodeDistribution(d: Distribution): String = distributionSchema.encodeString(d)

  def decodeDistribution(s: String): Result[Throwable, Distribution] =
    distributionSchema.decodeString(s)
