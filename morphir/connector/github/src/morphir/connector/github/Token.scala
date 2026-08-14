package morphir.connector.github

import kyo.*

/** A GitHub access token. Blank input is rejected rather than stored. */
opaque type Token = String

object Token:

  def parse(raw: String): Maybe[Token] =
    val trimmed = raw.trim
    if trimmed.isEmpty then Absent else Present(trimmed)

  extension (token: Token) def value: String = token
