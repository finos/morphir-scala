package org.finos.morphir.formats

import errors.FormatError.TextParseError
import zio.IO

object sources:

  trait TextSource[+Out]:
    def parseText(input: String): IO[TextParseError, Out]
