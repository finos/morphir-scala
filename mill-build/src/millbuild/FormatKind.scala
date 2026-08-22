package millbuild

/**
 * Which formatter families a `./mill format` (or shared check) invocation should run.
 *
 *   - [[All]] — scalafmt for `.scala` / `.mill` and elm-format for `.elm`
 *   - [[Scala]] — scalafmt only
 *   - [[Elm]] — elm-format only
 */
enum FormatKind {
  case All, Scala, Elm
}

object FormatKind {

  /** Parse a CLI / mainargs kind string. Unknown values fail with a short message. */
  def parse(value: String): Either[String, FormatKind] =
    value.trim.toLowerCase match {
      case "all"   => Right(All)
      case "scala" => Right(Scala)
      case "elm"   => Right(Elm)
      case other   =>
        Left(s"unknown format kind '$other' (expected all, scala, or elm)")
    }
}
