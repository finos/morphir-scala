package org.finos.morphir.mill

import upickle.default.{ReadWriter, readwriter}

opaque type ModuleId = String

object ModuleId {
  inline given CanEqual[ModuleId, ModuleId] = CanEqual.derived

  enum Error(
      val input: String,
      val location: SourceLocation,
      message: String
  ) extends IllegalArgumentException(s"$message at ${location.render}") {
    case Empty(override val location: SourceLocation)
        extends Error("", location, "Module ID cannot be empty")
    case EmptySegment(override val input: String, index: Int, override val location: SourceLocation)
        extends Error(input, location, s"Module ID segment $index cannot be empty")
    case InvalidSegment(
        override val input: String,
        index: Int,
        value: String,
        override val location: SourceLocation
    ) extends Error(input, location, s"Invalid module ID segment $index: $value")
    case ReservedSegment(
        override val input: String,
        index: Int,
        value: String,
        override val location: SourceLocation
    ) extends Error(input, location, s"Reserved module ID segment $index: $value")
  }

  private val PortableSegment         = "[a-z0-9](?:[a-z0-9_-]*[a-z0-9])?".r
  private val WindowsReservedSegments =
    Set("con", "prn", "aux", "nul") ++ (1 to 9).flatMap(number => Seq(s"com$number", s"lpt$number"))

  given ReadWriter[ModuleId] = readwriter[String].bimap[ModuleId](
    _.value,
    value => parse(value)(using SourceLocation.serialized).fold(throw _, identity)
  )

  def parse(value: String)(using location: SourceLocation): Either[Error, ModuleId] =
    validate(value, location)

  private[mill] def validate(value: String, location: SourceLocation): Either[Error, ModuleId] =
    if (value.isEmpty) Left(Error.Empty(location))
    else {
      val segments = value.split("\\.", -1).toIndexedSeq
      segments.zipWithIndex.collectFirst {
        case (segment, index) if segment.isEmpty =>
          Error.EmptySegment(value, index, location)
        case (segment, index) if !PortableSegment.matches(segment) =>
          Error.InvalidSegment(value, index, segment, location)
        case (segment, index) if WindowsReservedSegments.contains(segment) =>
          Error.ReservedSegment(value, index, segment, location)
      } match {
        case Some(error) => Left(error)
        case None        => Right(value)
      }
    }

  extension (self: ModuleId) {
    def value: String            = self
    def segments: IArray[String] = IArray.from(self.split("\\."))
  }
}

extension (inline context: StringContext)
  inline def moduleId(inline arguments: Any*): ModuleId =
    ${ ModuleIdInterpolator.expand('context, 'arguments) }
