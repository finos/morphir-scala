package org.finos.morphir.runtime.quick

import org.finos.morphir.naming._
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Value.{Pattern, Value}
import org.finos.morphir.ir.Value.Value.{List as ListValue, *}
import Helpers.{listToTuple, matchPatternCase, unpackLit}
import SDKValue.{SDKNativeFunction, SDKNativeValue}
import org.finos.morphir.runtime.{
  ConstructorNotFound,
  DefinitionNotFound,
  FunctionWithoutParameters,
  MissingField,
  UnexpectedType,
  UnmatchedPattern,
  VariableNotFound,
  Utils
}

object Loop {
  def loop[TA, VA](ir: Value[TA, VA], store: Store[TA, VA]): Result[TA, VA] =
    ir match {
      case Literal(va, lit)              => handleLiteral(va, lit)
      case Apply(va, function, argument) => handleApply(va, function, argument, store)
      case Destructure(va, pattern, valueToDestruct, inValue) =>
        handleDestructure(va, pattern, valueToDestruct, inValue, store)
      case Constructor(va, name)        => handleConstructor(va, name, store)
      case Field(va, recordValue, name) => handleField(va, recordValue, name, store)
      case FieldFunction(va, name)      => handleFieldFunction(va, name)
      case IfThenElse(va, condition, thenValue, elseValue) =>
        handleIfThenElse(va, condition, thenValue, elseValue, store)
      case Lambda(va, pattern, body) => handleLambda(va, pattern, body, store)
      case LetDefinition(va, name, definition, inValue) =>
        handleLetDefinition(va, name, definition, inValue, store)
      case LetRecursion(va, definitions, inValue)  => handleLetRecursion(va, definitions, inValue, store)
      case ListValue(va, elements)                 => handleListValue(va, elements.toList, store)
      case PatternMatch(va, value, cases)          => handlePatternMatch(va, value, cases.toList, store)
      case Record(va, fields)                      => handleRecord(va, fields.toList, store)
      case Reference(va, name)                     => handleReference(va, name, store)
      case Tuple(va, elements)                     => handleTuple(va, elements.toList, store)
      case Unit(va)                                => handleUnit(va)
      case UpdateRecord(va, valueToUpdate, fields) => handleUpdateRecord(va, valueToUpdate, fields, store)
      case Variable(va, name)                      => handleVariable(va, name, store)
    }

  def handleLiteral[TA, VA](va: VA, literal: Lit) = Result.Primitive[TA, VA](unpackLit(literal))

  def handleApply[TA, VA](
      va: VA,
      function: Value[TA, VA],
      argument: Value[TA, VA],
      store: Store[TA, VA]
  ): Result[TA, VA] = {
    val functionValue = loop(function, store)
    val argValue      = loop(argument, store)
    functionValue match {
      case Result.FieldFunction(name) =>
        argValue match {
          case Result.Record(fields) =>
            fields.getOrElse(name, throw MissingField(s"Record fields $fields do not contain name $name"))
          case other => throw UnexpectedType(s"Expected record but found $other")
        }
      case Result.LambdaFunction(body, pattern, context) =>
        val newBindings = matchPatternCase[TA, VA](pattern, argValue)
          .getOrElse(throw UnmatchedPattern(s"Lambda argument did not match expected pattern"))
          .map { case (name, value) => name -> StoredValue.Eager(value) }
        loop(body, Store(store.definitions, store.ctors, context.push(newBindings)))
      case Result.DefinitionFunction(body, arguments, curried, closingContext) =>
        arguments match {
          case (name, _, _) :: Nil =>
            val newBindings = (curried :+ (name -> argValue)).map { case (name, value) =>
              name -> StoredValue.Eager(value)
            }.toMap
            loop(body, Store(store.definitions, store.ctors, closingContext.push(newBindings)))
          case (name, _, _) :: tail =>
            Result.DefinitionFunction(body, tail, curried :+ (name -> argValue), closingContext)
          case Nil =>
            throw FunctionWithoutParameters(
              "Tried to apply definition function with no un-applied arguments (should not exist)"
            )

        }
      case Result.ConstructorFunction(name, arguments, curried) =>
        arguments match {
          case _ :: Nil  => Result.ConstructorResult[TA, VA](name, curried :+ argValue)
          case _ :: tail => Result.ConstructorFunction[TA, VA](name, tail, curried :+ argValue)
          case Nil =>
            throw FunctionWithoutParameters(
              s"Tried to apply to constructor function with no arguments (should not exist)"
            )

        }
      case Result.NativeFunction(arguments, curried, function) =>
        arguments match {
          case 1 =>
            curried.size match {
              case 0 => ((function.asInstanceOf[(Result[TA, VA]) => Result[TA, VA]])(argValue))
              case 1 => (
                (function.asInstanceOf[(Result[TA, VA], Result[TA, VA]) => Result[TA, VA]])(
                  curried(0),
                  argValue
                )
              )
              case 2 => (
                (function.asInstanceOf[(Result[TA, VA], Result[TA, VA], Result[TA, VA]) => Result[TA, VA]])(
                  curried(0),
                  curried(1),
                  argValue
                )
              )
            }
          case x => Result.NativeFunction[TA, VA](x - 1, curried :+ argValue, function)
        }
      case other => throw new UnexpectedType(s"$other is not a function")
    }
  }

  def handleDestructure[TA, VA](
      va: VA,
      pattern: Pattern[VA],
      valueToDestruct: Value[TA, VA],
      inValue: Value[TA, VA],
      store: Store[TA, VA]
  ): Result[TA, VA] = {
    val value = loop(valueToDestruct, store)
    matchPatternCase[TA, VA](pattern, value) match {
      case None => throw UnmatchedPattern(s"Value $value does not match pattern $pattern")
      case Some(bindings) =>
        loop(inValue, store.push(bindings.map { case (name, value) => name -> StoredValue.Eager(value) }))
    }
  }

  def handleConstructor[TA, VA](va: VA, name: FQName, store: Store[TA, VA]): Result[TA, VA] =
    store.getCtor(name) match {
      case Some(SDKConstructor(List()))    => Result.ConstructorResult(name, List())
      case Some(SDKConstructor(arguments)) => Result.ConstructorFunction[TA, VA](name, arguments, List())
      case None =>
        throw ConstructorNotFound(s"$name not found in constructor store. Store contents: ${store.ctors.keys}")
    }

  def handleField[TA, VA](
      va: VA,
      recordValue: Value[TA, VA],
      fieldName: Name,
      store: Store[TA, VA]
  ): Result[TA, VA] =
    loop(recordValue, store) match {
      case Result.Record(fields) =>
        fields.getOrElse(fieldName, throw MissingField(s"Record fields $fields do not contain name $fieldName"))
      case other => throw new UnexpectedType(s"Expected record but found $other")
    }

  def handleFieldFunction[TA, VA](va: VA, name: Name): Result[TA, VA] = Result.FieldFunction(name)

  def handleIfThenElse[TA, VA](
      va: VA,
      condition: Value[TA, VA],
      thenValue: Value[TA, VA],
      elseValue: Value[TA, VA],
      store: Store[TA, VA]
  ) =
    loop(condition, store) match {
      case Result.Primitive(true)  => loop(thenValue, store)
      case Result.Primitive(false) => loop(elseValue, store)
      case other                   => throw new UnexpectedType(s"$other is not a boolean result")
    }

  def handleLambda[TA, VA](
      va: VA,
      pattern: Pattern[VA],
      body: Value[TA, VA],
      store: Store[TA, VA]
  ): Result[TA, VA] =
    Result.LambdaFunction(body, pattern, store.callStack)

  def handleLetDefinition[TA, VA](
      va: VA,
      valueName: Name,
      valueDefinition: Definition[TA, VA],
      inValue: Value[TA, VA],
      store: Store[TA, VA]
  ) = {
    val value =
      if (valueDefinition.inputTypes.isEmpty) loop(valueDefinition.body, store)
      else
        Result.DefinitionFunction(valueDefinition.body, valueDefinition.inputTypes.toList, List(), store.callStack)
    loop(inValue, store.push(Map(valueName -> StoredValue.Eager(value))))
  }

  def handleLetRecursion[TA, VA](
      va: VA,
      definitions: Map[Name, Definition[TA, VA]],
      inValue: Value[TA, VA],
      store: Store[TA, VA]
  ): Result[TA, VA] = {
    val siblings = definitions.map { case (name, definition) =>
      name -> StoredValue.Lazy(definition, store.callStack, definitions)
    }
    loop(inValue, store.push(siblings))
  }

  def handleListValue[TA, VA](va: VA, elements: List[Value[TA, VA]], store: Store[TA, VA]): Result[TA, VA] =
    Result.ListResult(elements.map(loop(_, store)))

  def handlePatternMatch[TA, VA](
      va: VA,
      value: Value[TA, VA],
      cases: List[(Pattern[VA], Value[TA, VA])],
      store: Store[TA, VA]
  ): Result[TA, VA] = {
    val evaluated = loop(value, store)

    def firstPatternMatching(
        remainingCases: List[(Pattern[VA], Value[TA, VA])]
    ): (Value[TA, VA], Map[Name, Result[TA, VA]]) =
      remainingCases match {
        case (pattern, inValue) :: tail =>
          matchPatternCase(pattern, evaluated).map((inValue, _)).getOrElse(firstPatternMatching(tail))
        case Nil => throw UnmatchedPattern(s"$evaluated did not match any pattern from $cases")
      }

    val (inValue, bindings) = firstPatternMatching(cases)
    loop(inValue, store.push(bindings.map { case (name, value) => name -> StoredValue.Eager(value) }))
  }

  def handleRecord[TA, VA](va: VA, fields: List[(Name, Value[TA, VA])], store: Store[TA, VA]): Result[TA, VA] =
    Result.Record(fields.map { case (name, value) => name -> loop(value, store) }.toMap)

  def handleReference[TA, VA](va: VA, name: FQName, store: Store[TA, VA]): Result[TA, VA] =
    store.getDefinition(name) match {
      case None =>
        val filtered = store.definitions.keys.filter(_.getPackagePath == name.getPackagePath)
        val hint = if (Utils.isNative(name)) "You might be calling an unimplemented native function"
        else "You might be calling a function not defined in the given distributions"
        throw DefinitionNotFound(
          s"""name $name not found in store.
             | Hint: $hint
             | For that package, store contains:
             | \t${filtered.map(_.toString).mkString("\n\t")}""".stripMargin


        )
      case Some(SDKValue.SDKValueDefinition(valueDefinition)) =>
        if (valueDefinition.inputTypes.isEmpty) {
          loop(valueDefinition.body, store)
        } else
          Result.DefinitionFunction(
            valueDefinition.body,
            valueDefinition.inputTypes.toList,
            List(),
            store.callStack
          )
      case Some(SDKNativeValue(value))                  => value
      case Some(SDKNativeFunction(arguments, function)) => Result.NativeFunction(arguments, List(), function)
    }

  def handleTuple[TA, VA](va: VA, elements: List[Value[TA, VA]], store: Store[TA, VA]): Result[TA, VA] = {
    val evaluatedElements = elements.map(loop(_, store))
    Result.Tuple(listToTuple(evaluatedElements))
  }

  def handleUnit[TA, VA](va: VA): Result[TA, VA] = Result.Unit()

  def handleUpdateRecord[TA, VA](
      va: VA,
      valueToUpdate: Value[TA, VA],
      fields: Map[Name, Value[TA, VA]],
      store: Store[TA, VA]
  ): Result[TA, VA] =
    loop(valueToUpdate, store) match {
      case Result.Record(oldFields) =>
        val newFields = fields.map { case (name, value) => name -> loop(value, store) }
        Result.Record(oldFields ++ newFields)
      case other => throw UnexpectedType(s"$other is not a record")
    }

  def handleVariable[TA, VA](va: VA, name: Name, store: Store[TA, VA]) =
    store.getVariable(name) match {
      case None                         => throw VariableNotFound(s"Variable $name not found")
      case Some(StoredValue.Eager(res)) => res
      case Some(StoredValue.Lazy(definition, parentContext, siblings)) =>
        val newBindings: Map[Name, StoredValue[TA, VA]] = siblings.map { case (name, sibling) =>
          name -> StoredValue.Lazy(sibling, parentContext, siblings)
        }
        if (definition.inputTypes.isEmpty)
          loop(definition.body, store.push(newBindings))
        else
          Result.DefinitionFunction(
            definition.body,
            definition.inputTypes.toList,
            List(),
            store.push(newBindings).callStack
          )
    }
}
