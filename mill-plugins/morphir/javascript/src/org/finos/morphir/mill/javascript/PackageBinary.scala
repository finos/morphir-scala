package org.finos.morphir.mill.javascript

import java.util.Locale

opaque type PackageBinary = String

object PackageBinary {
  final case class CallSite(file: String, line: Int, enclosing: String) {
    def render: String = s"$file:$line in $enclosing"
  }

  object CallSite {
    given generated(using file: sourcecode.File, line: sourcecode.Line, enclosing: sourcecode.FullName): CallSite =
      CallSite(file.value, line.value, enclosing.value)
  }

  final case class Error(input: String, location: CallSite)
      extends IllegalArgumentException(s"Invalid package binary '$input' at ${location.render}")

  private val PortableName         = "[A-Za-z0-9._-]+".r
  private val WindowsReservedNames =
    Set("CON", "PRN", "AUX", "NUL") ++ (1 to 9).flatMap(number => Seq(s"COM$number", s"LPT$number"))

  def parse(value: String)(using location: CallSite): Either[Error, PackageBinary] =
    validate(value, location)

  private[javascript] def validate(value: String, location: CallSite): Either[Error, PackageBinary] = {
    val basename = value.takeWhile(_ != '.').toUpperCase(Locale.ROOT)
    value match {
      case PortableName() if value != "." && value != ".." && !WindowsReservedNames.contains(basename) => Right(value)
      case _ => Left(Error(value, location))
    }
  }

  extension (self: PackageBinary) def value: String = self
}

extension (inline context: StringContext)
  inline def packageBinary(inline arguments: Any*): PackageBinary =
    ${ PackageBinaryInterpolator.expand('context, 'arguments) }
