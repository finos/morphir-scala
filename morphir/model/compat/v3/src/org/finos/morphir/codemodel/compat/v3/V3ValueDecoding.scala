package org.finos.morphir.codemodel.compat.v3

import kyo.Chunk
import kyo.Structure
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.Name

import scala.math.Ordering.Implicits.seqOrdering

/**
 * Decoders for v3 patterns, values, value definitions and value specifications.
 *
 * A value's or pattern's attribute (argument zero of each tagged node) follows the v3 lowering: a typed value carries a
 * v3 type there, which becomes the code-model node's `inferredType`; any other attribute - morphir-elm writes `{}` for
 * an untyped value - is erased to `ValueAttributes.empty`. An attribute is taken to be a type when it is a non-empty
 * array that starts with a tag string, and is then decoded as one, so a malformed type is a fault rather than silently
 * dropped.
 */
private[v3] object V3ValueDecoding:
  import V3JsonShape.*
  import V3NamingDecoding.*
  import V3TypeDecoding.*
  import V3Reader.Read

  def valueAttributes(value: Value, path: String): Read[cm.ValueAttributes] =
    value match
      case Structure.Value.Sequence(items) if items.nonEmpty && isTag(items.head) =>
        tpe(value, path).map(inferred => cm.ValueAttributes.empty.copy(inferredType = Some(inferred)))
      case _ => succeed(cm.ValueAttributes.empty)

  private def isTag(value: Value): Boolean =
    value match
      case Structure.Value.Str(_) => true
      case _                      => false

  def pattern(value: Value, path: String): Read[cm.Pattern] =
    tagged(value, path) { (tag, args) =>
      def arg(index: Int): String                                                           = at(path, index + 1)
      def node(arity: Int)(build: cm.ValueAttributes => Read[cm.Pattern]): Read[cm.Pattern] =
        arguments(tag, args, arity, path)(valueAttributes(args(0), arg(0)).flatMap(build))
      tag match
        case "WildcardPattern" => node(1)(attributes => succeed(cm.Pattern.WildcardPattern(attributes)))
        case "AsPattern"       =>
          node(3) { attributes =>
            for
              nested <- pattern(args(1), arg(1))
              alias  <- name(args(2), arg(2))
            yield cm.Pattern.AsPattern(attributes, nested, alias)
          }
        case "TuplePattern" =>
          node(2)(attributes => list(args(1), arg(1))(pattern).map(cm.Pattern.TuplePattern(attributes, _)))
        case "ConstructorPattern" =>
          node(3) { attributes =>
            for
              constructor <- fqName(args(1), arg(1))
              patterns    <- list(args(2), arg(2))(pattern)
            yield cm.Pattern.ConstructorPattern(attributes, constructor, patterns)
          }
        case "EmptyListPattern" => node(1)(attributes => succeed(cm.Pattern.EmptyListPattern(attributes)))
        case "HeadTailPattern"  =>
          node(3) { attributes =>
            for
              head <- pattern(args(1), arg(1))
              tail <- pattern(args(2), arg(2))
            yield cm.Pattern.HeadTailPattern(attributes, head, tail)
          }
        case "LiteralPattern" =>
          node(2)(attributes => literal(args(1), arg(1)).map(cm.Pattern.LiteralPattern(attributes, _)))
        case "UnitPattern" => node(1)(attributes => succeed(cm.Pattern.UnitPattern(attributes)))
        case other         => unknownTag(path, other)
    }

  def expr(value: Value, path: String): Read[cm.Expr] =
    tagged(value, path) { (tag, args) =>
      def arg(index: Int): String                                                     = at(path, index + 1)
      def node(arity: Int)(build: cm.ValueAttributes => Read[cm.Expr]): Read[cm.Expr] =
        arguments(tag, args, arity, path)(valueAttributes(args(0), arg(0)).flatMap(build))
      tag match
        case "Literal" =>
          node(2)(attributes => literal(args(1), arg(1)).map(cm.Expr.Literal(attributes, _)))
        case "Constructor" =>
          node(2)(attributes => fqName(args(1), arg(1)).map(cm.Expr.Constructor(attributes, _)))
        case "Tuple" =>
          node(2)(attributes => list(args(1), arg(1))(expr).map(cm.Expr.Tuple(attributes, _)))
        case "List" =>
          node(2)(attributes => list(args(1), arg(1))(expr).map(cm.Expr.List(attributes, _)))
        case "Record" =>
          node(2)(attributes => list(args(1), arg(1))(recordField).map(cm.Expr.Record(attributes, _)))
        case "Unit"     => node(1)(attributes => succeed(cm.Expr.Unit(attributes)))
        case "Variable" =>
          node(2)(attributes => name(args(1), arg(1)).map(cm.Expr.Variable(attributes, _)))
        case "Reference" =>
          node(2)(attributes => fqName(args(1), arg(1)).map(cm.Expr.Reference(attributes, _)))
        case "Field" =>
          node(3) { attributes =>
            for
              subject   <- expr(args(1), arg(1))
              fieldName <- name(args(2), arg(2))
            yield cm.Expr.Field(attributes, subject, fieldName)
          }
        case "FieldFunction" =>
          node(2)(attributes => name(args(1), arg(1)).map(cm.Expr.FieldFunction(attributes, _)))
        case "Apply" =>
          node(3) { attributes =>
            for
              function <- expr(args(1), arg(1))
              argument <- expr(args(2), arg(2))
            yield cm.Expr.Apply(attributes, function, argument)
          }
        case "Lambda" =>
          node(3) { attributes =>
            for
              argumentPattern <- pattern(args(1), arg(1))
              body            <- expr(args(2), arg(2))
            yield cm.Expr.Lambda(attributes, argumentPattern, body)
          }
        case "LetDefinition" =>
          node(4) { attributes =>
            for
              valueName  <- name(args(1), arg(1))
              definition <- valueDefinitionBody(args(2), arg(2))
              inValue    <- expr(args(3), arg(3))
            yield cm.Expr.LetDefinition(attributes, valueName, definition, inValue)
          }
        case "LetRecursion" =>
          node(3) { attributes =>
            for
              definitions <-
                list(args(1), arg(1))((entry, entryPath) => pair(entry, entryPath)(name, valueDefinitionBody))
              inValue <- expr(args(2), arg(2))
            yield cm.Expr.LetRecursion(
              attributes,
              byName(definitions).map((bindingName, definition) => cm.Binding(bindingName, definition)),
              inValue
            )
          }
        case "Destructure" =>
          node(4) { attributes =>
            for
              destructuring <- pattern(args(1), arg(1))
              subject       <- expr(args(2), arg(2))
              inValue       <- expr(args(3), arg(3))
            yield cm.Expr.Destructure(attributes, destructuring, subject, inValue)
          }
        case "IfThenElse" =>
          node(4) { attributes =>
            for
              condition  <- expr(args(1), arg(1))
              thenBranch <- expr(args(2), arg(2))
              elseBranch <- expr(args(3), arg(3))
            yield cm.Expr.IfThenElse(attributes, condition, thenBranch, elseBranch)
          }
        case "PatternMatch" =>
          node(3) { attributes =>
            for
              subject <- expr(args(1), arg(1))
              cases   <- list(args(2), arg(2))(matchCase)
            yield cm.Expr.PatternMatch(attributes, subject, cases)
          }
        case "UpdateRecord" =>
          node(3) { attributes =>
            for
              subject <- expr(args(1), arg(1))
              updates <- list(args(2), arg(2))((entry, entryPath) => pair(entry, entryPath)(name, expr))
            yield cm.Expr.UpdateRecord(
              attributes,
              subject,
              byName(updates).map((fieldName, fieldValue) => cm.RecordField(fieldName, fieldValue))
            )
          }
        case other => unknownTag(path, other)
    }

  /**
   * v3 holds `LetRecursion` definitions and `UpdateRecord` fields in maps keyed by name, so input order carries no
   * meaning and a repeated name keeps its last entry. They are ordered by name, word by word, as the v3 lowering does.
   */
  private def byName[A](entries: Chunk[(Name, A)]): Chunk[(Name, A)] =
    Chunk.from(entries.toMap.toSeq.sortBy(_._1.toList))

  /** A record literal's field, `[name, value]`; record literals keep their order and any repeated name. */
  private def recordField(value: Value, path: String): Read[cm.RecordField] =
    pair(value, path)(name, expr).map((fieldName, fieldValue) => cm.RecordField(fieldName, fieldValue))

  private def matchCase(value: Value, path: String): Read[cm.MatchCase] =
    pair(value, path)(pattern, expr).map((casePattern, body) => cm.MatchCase(casePattern, body))

  /**
   * A v3 value definition, `{"inputTypes": [[name, attribute, type], …], "outputType": …, "body": …}`. Each input's
   * attribute is erased, as in the v3 lowering; v3 bodies are always expressions.
   */
  def valueDefinitionBody(value: Value, path: String): Read[cm.ValueDefinitionBody] =
    obj(value, path, "an object with inputTypes, outputType and body") { members =>
      for
        inputs <- members.read("inputTypes")((inputTypes, inputsPath) => list(inputTypes, inputsPath)(inputType))
        output <- members.read("outputType")(tpe)
        body   <- members.read("body")(expr)
      yield cm.ValueDefinitionBody.ExpressionBody(inputs, output, body)
    }

  private def inputType(value: Value, path: String): Read[cm.Parameter] =
    tuple(value, path, 3).flatMap { parts =>
      for
        inputName <- name(parts(0), at(path, 0))
        inputType <- tpe(parts(2), at(path, 2))
      yield cm.Parameter(inputName, inputType)
    }

  /**
   * A value definition under the given access. v3 records access once, on the module's entry for the value; the code
   * model also wraps the body in its own access, which takes the same level, as in the v3 lowering.
   */
  def valueDefinition(access: cm.Access)(value: Value, path: String): Read[cm.ValueDefinition] =
    valueDefinitionBody(value, path).map(body => cm.ValueDefinition(cm.AccessControlled(access, body)))

  /** A v3 value specification, `{"inputs": [[name, type], …], "output": …}`. */
  def valueSpecification(value: Value, path: String): Read[cm.ValueSpecification] =
    obj(value, path, "an object with inputs and output") { members =>
      for
        inputs <- members.read("inputs")((inputValues, inputsPath) => list(inputValues, inputsPath)(parameter))
        output <- members.read("output")(tpe)
      yield cm.ValueSpecification(inputs, output)
    }
end V3ValueDecoding
