package org.finos.morphir
package runtime

import org.finos.morphir.ir.internal.*
import org.finos.morphir.ir.internal.Value.{List as ListValue, *}
import org.finos.morphir.ir.Value.Pattern
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Literal.Literal.*
import org.finos.morphir.ir.{FQName, Name, Type}
import Helpers.*
import org.finos.morphir.runtime.SDKValue.SDKNativeFunction

object EvaluatorQuick {

  def evaluate[TA, VA](ir: Value[TA, VA], store: Store[TA, VA]): Any = ResultValue.unwrap(loop(ir, store))

  def loop[TA, VA](ir: Value[TA, VA], store: Store[TA, VA]): ResultValue[TA, VA] =
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

  def handleLiteral[TA, VA](va: VA, literal: Lit) = ResultValue.Primitive[TA, VA](unpackLit(literal))

  def handleApply[TA, VA](
      va: VA,
      function: Value[TA, VA],
      argument: Value[TA, VA],
      store: Store[TA, VA]
  ): ResultValue[TA, VA] = {
    val functionValue = loop(function, store)
    val argValue      = loop(argument, store)
    functionValue match {
      case ResultValue.FieldFunction(name) =>
        argValue match {
          case ResultValue.RecordResult(fields) =>
            fields.getOrElse(name, throw new Exception(s"Record fields $fields does not contain name $name"))
          case other => throw new Exception(s"Expected record but found $other")
        }
      case ResultValue.LambdaFunction(body, pattern, context) =>
        val newBindings = matchPatternCase[TA, VA](pattern, argValue)
          .getOrElse(throw new Exception(s"Lambda argument did not match expected pattern"))
          .map { case (name, value) => name -> StoredValue.Eager(value) }
        loop(body, Store(store.fqNameBindings, context.push(newBindings)))
      case ResultValue.DefinitionFunction(body, arguments, curied, closingContext) =>
        arguments match {
          case (name, _, _) :: Nil =>
            val newBindings = (curied :+ (name -> argValue)).map { case (name, value) =>
              name -> StoredValue.Eager(value)
            }.toMap
            loop(body, Store(store.fqNameBindings, closingContext.push(newBindings)))
          case (name, _, _) :: tail =>
            ResultValue.DefinitionFunction(body, tail, curied :+ (name -> argValue), closingContext)
          case Nil =>
            throw new Exception("Tried to apply definition function with no un-applied arguments (should not exist)")

        }
      case ResultValue.ConstructorFunction(name, arguments, curried) =>
        arguments match {
          case _ :: Nil  => ResultValue.ConstructorResult[TA, VA](name, curried :+ argValue)
          case _ :: tail => ResultValue.ConstructorFunction[TA, VA](name, tail, curried :+ argValue)
          case Nil =>
            throw new Exception(s"Tried to apply to constructor function with no arguments (should not exist)")

        }
      case ResultValue.NativeFunction(arguments, curried, function) =>
        arguments match {
          case 1 =>
            curried.size match {
              case 0 => ((function.asInstanceOf[(ResultValue[TA, VA]) => ResultValue[TA, VA]])(argValue))
              case 1 => (
                (function.asInstanceOf[(ResultValue[TA, VA], ResultValue[TA, VA]) => ResultValue[TA, VA]])(
                  curried(0),
                  argValue
                )
              )
            }
          case x => ResultValue.NativeFunction[TA, VA](x - 1, curried :+ argValue, function)
//          case Nil =>
//            throw new Exception(s"Tried to apply to native function with no arguments (should not exist)")
        }
      case other => throw new Exception(s"$other is not a function")
    }
  }

  def handleDestructure[TA, VA](
      va: VA,
      pattern: Pattern[VA],
      valueToDestruct: Value[TA, VA],
      inValue: Value[TA, VA],
      store: Store[TA, VA]
  ): ResultValue[TA, VA] = {
    val value = loop(valueToDestruct, store)
    matchPatternCase[TA, VA](pattern, value) match {
      case None => throw new Exception(s"Value $value does not match pattern $pattern")
      case Some(bindings) =>
        loop(inValue, store.push(bindings.map { case (name, value) => name -> StoredValue.Eager(value) }))
    }
  }

  def handleConstructor[TA, VA](va: VA, name: FQName, store: Store[TA, VA]): ResultValue[TA, VA] =
    store.get(name) match {
      case Some(SDKValue.SDKConstructor(arguments)) => ResultValue.ConstructorFunction[TA, VA](name, arguments, List())
      case Some(other) => throw new Exception(s"$name points to $other, which is not a constructor")
      case None        => throw new Exception(s"$name not found in store")
    }

  def handleField[TA, VA](
      va: VA,
      recordValue: Value[TA, VA],
      fieldName: Name,
      store: Store[TA, VA]
  ): ResultValue[TA, VA] =
    loop(recordValue, store) match {
      case ResultValue.RecordResult(fields) =>
        fields.getOrElse(fieldName, throw new Exception(s"Record fields $fields does not contain name $fieldName"))
      case other => throw new Exception(s"Expected record but found $other")
    }

  def handleFieldFunction[TA, VA](va: VA, name: Name): ResultValue[TA, VA] = ResultValue.FieldFunction(name)

  def handleIfThenElse[TA, VA](
      va: VA,
      condition: Value[TA, VA],
      thenValue: Value[TA, VA],
      elseValue: Value[TA, VA],
      store: Store[TA, VA]
  ) =
    loop(condition, store) match {
      case ResultValue.Primitive(true)  => loop(thenValue, store)
      case ResultValue.Primitive(false) => loop(elseValue, store)
      case other                        => throw new Exception(s"$other is not a boolean result")
    }

  def handleLambda[TA, VA](
      va: VA,
      pattern: Pattern[VA],
      body: Value[TA, VA],
      store: Store[TA, VA]
  ): ResultValue[TA, VA] =
    ResultValue.LambdaFunction(body, pattern, store.callStack)

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
        ResultValue.DefinitionFunction(valueDefinition.body, valueDefinition.inputTypes.toList, List(), store.callStack)
    loop(inValue, store.push(Map(valueName -> StoredValue.Eager(value))))
  }

  def handleLetRecursion[TA, VA](
      va: VA,
      definitions: Map[Name, Definition[TA, VA]],
      inValue: Value[TA, VA],
      store: Store[TA, VA]
  ): ResultValue[TA, VA] = {
    val siblings = definitions.map { case (name, definition) =>
      name -> StoredValue.Lazy(definition, store.callStack, definitions)
    }
    loop(inValue, store.push(siblings))
  }
  def handleListValue[TA, VA](va: VA, elements: List[Value[TA, VA]], store: Store[TA, VA]): ResultValue[TA, VA] =
    ResultValue.ListResult(elements.map(loop(_, store)))

  def handlePatternMatch[TA, VA](
      va: VA,
      value: Value[TA, VA],
      cases: List[(Pattern[VA], Value[TA, VA])],
      store: Store[TA, VA]
  ): ResultValue[TA, VA] = {
    // TODO: Eliminate extra recursion
    val evaluated = loop(value, store)
    def firstPatternMatching(
        cases: List[(Pattern[VA], Value[TA, VA])]
    ): (Value[TA, VA], Map[Name, ResultValue[TA, VA]]) =
      cases match {
        case (pattern, inValue) :: tail =>
          matchPatternCase(pattern, evaluated).map((inValue, _)).getOrElse(firstPatternMatching(tail))
        case Nil => throw new Exception(s"$value did not match any pattern from $cases")
      }
    val (inValue, bindings) = firstPatternMatching(cases)
    loop(inValue, store.push(bindings.map { case (name, value) => name -> StoredValue.Eager(value) }))
  }

  def handleRecord[TA, VA](va: VA, fields: List[(Name, Value[TA, VA])], store: Store[TA, VA]): ResultValue[TA, VA] =
    ResultValue.RecordResult(fields.map { case (name, value) => name -> loop(value, store) }.toMap)

  def handleReference[TA, VA](va: VA, name: FQName, store: Store[TA, VA]): ResultValue[TA, VA] =
    store.get(name) match {
      case None => throw new Exception(s"name $name not found in store")
      case Some(SDKValue.SDKValueDefinition(valueDefinition)) =>
        if (valueDefinition.inputTypes.isEmpty) {
          loop(valueDefinition.body, store)
        } else
          ResultValue.DefinitionFunction(
            valueDefinition.body,
            valueDefinition.inputTypes.toList,
            List(),
            store.callStack
          )
      case Some(SDKNativeFunction(arguments, function)) => ResultValue.NativeFunction(arguments, List(), function)
      case Some(other) =>
        throw new Exception(s"$name points to $other, which is not a value definition (is it a type definition?)")
    }
  def handleTuple[TA, VA](va: VA, elements: List[Value[TA, VA]], store: Store[TA, VA]): ResultValue[TA, VA] = {
    val evaluatedElements = elements.map(loop(_, store))
    ResultValue.TupleResult(listToTuple(evaluatedElements))
  }
  def handleUnit[TA, VA](va: VA): ResultValue[TA, VA] = ResultValue.Unit()

  def handleUpdateRecord[TA, VA](
      va: VA,
      valueToUpdate: Value[TA, VA],
      fields: Map[Name, Value[TA, VA]],
      store: Store[TA, VA]
  ): ResultValue[TA, VA] =
    loop(valueToUpdate, store) match {
      case ResultValue.RecordResult(oldFields) =>
        val newFields = fields.map { case (name, value) => name -> loop(value, store) }
        ResultValue.RecordResult(oldFields ++ newFields)
      case other => throw new Exception(s"$other is not a record")
    }

  def handleVariable[TA, VA](va: VA, name: Name, store: Store[TA, VA]) =
    store.get(name) match {
      case None                         => throw new Exception(s"Variable $name not found")
      case Some(StoredValue.Eager(res)) => res
      case Some(StoredValue.Lazy(definition, parentContext, siblings)) =>
        val newBindings: Map[Name, StoredValue[TA, VA]] = siblings.map { case (name, sibling) =>
          name -> StoredValue.Lazy(sibling, parentContext, siblings)
        }
        if (definition.inputTypes.isEmpty)
          loop(definition.body, store.push(newBindings))
        else
          ResultValue.DefinitionFunction(
            definition.body,
            definition.inputTypes.toList,
            List(),
            store.push(newBindings).callStack
          )
    }

}
