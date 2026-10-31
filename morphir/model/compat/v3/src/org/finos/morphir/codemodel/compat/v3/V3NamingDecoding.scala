package org.finos.morphir.codemodel.compat.v3

import kyo.Structure
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.FQName
import org.finos.morphir.naming.ModuleName
import org.finos.morphir.naming.Name
import org.finos.morphir.naming.PackageName
import org.finos.morphir.naming.Path

/** Decoders for v3 names, paths, fully qualified names and literals. */
private[v3] object V3NamingDecoding:
  import V3JsonShape.*
  import V3Reader.Read

  /** A name is an array of lower-case words: `["value", "in", "u", "s", "d"]`. Words are kept as written. */
  def name(value: Value, path: String): Read[Name] =
    list(value, path)(string).map(words => Name.fromList(words.toList))

  /** A path is an array of names: `[["morphir"], ["s", "d", "k"]]`. */
  def namingPath(value: Value, path: String): Read[Path] =
    list(value, path)(name).map(names => Path.fromList(names.toList))

  def packageName(value: Value, path: String): Read[PackageName] = namingPath(value, path).map(PackageName(_))

  def moduleName(value: Value, path: String): Read[ModuleName] = namingPath(value, path).map(ModuleName(_))

  /** A fully qualified name is `[packagePath, modulePath, localName]`. */
  def fqName(value: Value, path: String): Read[FQName] =
    tuple(value, path, 3).flatMap { parts =>
      for
        packagePath <- packageName(parts(0), at(path, 0))
        modulePath  <- moduleName(parts(1), at(path, 1))
        localName   <- name(parts(2), at(path, 2))
      yield FQName(packagePath, modulePath, localName)
    }

  /**
   * A literal is a tagged array with one argument. `WholeNumberLiteral` must fit in 64 bits, as in v3; a `CharLiteral`
   * must hold exactly one code point; a `DecimalLiteral` is the decimal's text (a JSON number is also accepted).
   */
  def literal(value: Value, path: String): Read[cm.Literal] =
    tagged(value, path) { (tag, args) =>
      val argumentPath                                             = at(path, 1)
      def one(decode: Value => Read[cm.Literal]): Read[cm.Literal] =
        arguments(tag, args, 1, path)(decode(args(0)))
      tag match
        case "BoolLiteral" =>
          one {
            case Structure.Value.Bool(flag) => succeed(cm.Literal.BoolLiteral(flag))
            case _                          => wrongShape(argumentPath, "true or false")
          }
        case "CharLiteral" =>
          one {
            case Structure.Value.Str(text) if text.codePointCount(0, text.length) == 1 =>
              succeed(cm.Literal.CharLiteral(text))
            case _ => wrongShape(argumentPath, "a string of one character")
          }
        case "StringLiteral"      => one(string(_, argumentPath).map(cm.Literal.StringLiteral(_)))
        case "WholeNumberLiteral" =>
          one {
            case Structure.Value.Integer(number) => succeed(cm.Literal.IntegerLiteral(BigInt(number)))
            case Structure.Value.BigNum(number) if number.isValidLong =>
              succeed(cm.Literal.IntegerLiteral(number.toBigInt))
            case _ => wrongShape(argumentPath, "a whole number in the 64-bit range")
          }
        case "FloatLiteral" =>
          one {
            case Structure.Value.Decimal(number) => succeed(cm.Literal.FloatLiteral(number))
            case Structure.Value.Integer(number) => succeed(cm.Literal.FloatLiteral(number.toDouble))
            case Structure.Value.BigNum(number)  => succeed(cm.Literal.FloatLiteral(number.toDouble))
            case _                               => wrongShape(argumentPath, "a number")
          }
        case "DecimalLiteral" =>
          one {
            case Structure.Value.Str(text)       => decimal(text, argumentPath)
            case Structure.Value.Integer(number) => succeed(cm.Literal.DecimalLiteral(BigDecimal(number)))
            case Structure.Value.Decimal(number) => decimal(number.toString, argumentPath)
            case Structure.Value.BigNum(number)  => succeed(cm.Literal.DecimalLiteral(number))
            case _                               => wrongShape(argumentPath, "a decimal number written as a string")
          }
        case other => unknownTag(path, other)
    }

  private def decimal(text: String, path: String): Read[cm.Literal] =
    try succeed(cm.Literal.DecimalLiteral(BigDecimal(new java.math.BigDecimal(text))))
    catch case _: NumberFormatException => wrongShape(path, "a decimal number")
end V3NamingDecoding
