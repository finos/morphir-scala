package org.finos.morphir.formats.internal.json

import org.finos.morphir.formats.sources.TextSource
import zio.{IO, ZIO}
import org.finos.morphir.formats.errors.FormatError
import org.finos.morphir.formats.errors.FormatError.TextParseError
import org.finos.morphir.formats.text.Text
import zio.json._

class JsonSource[+A](using JsonDecoder[A]) extends TextSource[A]:

  override def parseText(input: String): IO[TextParseError, A] =
    input
      .fromJson[A]
      .fold(
        err => ZIO.fail(FormatError.textParseError(err, Text.Plain(input))),
        ZIO.succeed(_)
      )
