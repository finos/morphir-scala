package org.finos.morphir.runtime.quick

import org.finos.morphir.naming.*
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Value.{Pattern, Value}
import org.finos.morphir.ir.Value.Value.{List as ListValue, *}
import Helpers.{listToTuple, matchPatternCase, unpackLit}
import org.finos.morphir.runtime.SDKValue.{SDKNativeFunction, SDKNativeInnerFunction, SDKNativeValue}
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.runtime.TypedMorphirRuntimeDefs.{RuntimeDefinition, RuntimeValue, TypeAttribs, ValueAttribs}
import org.finos.morphir.runtime.internal.{NativeFunctionSignature, StoredValue}
import org.finos.morphir.runtime.internal.InvokeableEvaluator
import org.finos.morphir.ir.printing.{DetailLevel, PrintIR}
import org.finos.morphir.runtime.{RTValue, SDKConstructor, SDKValue, Utils}
import org.finos.morphir.runtime.MorphirRuntimeError.{
  ConstructorNotFound,
  DefinitionNotFound,
  InvalidState,
  MissingField,
  UnexpectedType,
  UnmatchedPattern,
  VariableNotFound,
  WrongNumberOfArguments
}
import org.finos.morphir.util.PrintRTValue

private[morphir] case class Loop(globals: GlobalDefs) extends InvokeableEvaluator {
  def loop(ir: RuntimeValue, store: Store): RTValue = {
    val result = ir match {
      case Literal(va, lit)              => handleLiteral(va, lit)
      case Apply(va, function, argument) => handleApply(va, function, argument, store)
      case node @ Destructure(va, pattern, valueToDestruct, inValue) =>
        handleDestructure(va, node, pattern, valueToDestruct, inValue, store)
      case Constructor(va, name)        => handleConstructor(va, name)
      case Field(va, recordValue, name) => handleField(va, recordValue, name, store)
      case FieldFunction(va, name)      => handleFieldFunction(va, name)
      case IfThenElse(va, condition, thenValue, elseValue) =>
        handleIfThenElse(va, condition, thenValue, elseValue, store)
      case Lambda(va, pattern, body) => handleLambda(va, pattern, body, store)
      case LetDefinition(va, name, definition, inValue) =>
        handleLetDefinition(va, name, definition, inValue, store)
      case LetRecursion(va, definitions, inValue)  => handleLetRecursion(va, definitions, inValue, store)
      case ListValue(va, elements)                 => handleListValue(va, elements.toList, store)
      case node @ PatternMatch(va, value, cases)   => handlePatternMatch(va, node, value, cases.toList, store)
      case Record(va, fields)                      => handleRecord(va, fields.toList, store)
      case Reference(va, name)                     => handleReference(va, name, store)
      case Tuple(va, elements)                     => handleTuple(va, elements.toList, store)
      case Unit(va)                                => handleUnit(va)
      case UpdateRecord(va, valueToUpdate, fields) => handleUpdateRecord(va, valueToUpdate, fields, store)
      case Variable(va, name)                      => handleVariable(va, name, store)
    }
    result match {
      case _ => println(s"Loop sees: ${PrintRTValue(result).plainText}")
    }
    result
  }

  def handleLiteral(va: ValueAttribs, literal: Lit) = unpackLit(literal)

  def handleApply(
      va: ValueAttribs,
      function: RuntimeValue,
      argument: RuntimeValue,
      store: Store
  ): RTValue = {
    val functionValue = loop(function, store)
    val argValue      = loop(argument, store)
    handleApplyResult(va, functionValue, argValue)
  }

  def handleApplyResult2(
      va: ValueAttribs,
      functionValue: RTValue,
      arg1: RTValue,
      arg2: RTValue
  ): RTValue = {
    val partiallyAppliedFunction =
      handleApplyResult(va, functionValue, arg1)
    val result =
      handleApplyResult(va, partiallyAppliedFunction, arg2)
    result
  }

  def handleApplyResult(
      va: ValueAttribs,
      functionValue: RTValue,
      argValue: RTValue
  ): RTValue =
    functionValue match {
      case RTValue.FieldFunction(name) =>
        argValue match {
          case record @ RTValue.Record(fields) =>
            fields.getOrElse(name, throw MissingField(record, name))
          case other => throw UnexpectedType(
              "Record",
              other,
              hint = "Expected because this value was passed as an argument to a field function "
            )
        }
      case RTValue.LambdaFunction(body, pattern, context) =>
        val newBindings = matchPatternCase(pattern, argValue)
          .getOrElse(throw UnmatchedPattern(
            argValue,
            functionValue,
            pattern
          ))
          .map { case (name, value) => name -> StoredValue.Eager(value) }
        loop(body, Store(context.push(newBindings)))
      case function @ RTValue.DefinitionFunction(body, arguments, curried, closingContext) =>
        arguments match {
          case (name, _, _) :: Nil =>
            val newBindings = (curried :+ (name -> argValue)).map { case (name, value) =>
              name -> StoredValue.Eager(value)
            }.toMap
            loop(body, Store(closingContext.push(newBindings)))
          case (name, _, _) :: tail =>
            RTValue.DefinitionFunction(body, tail, curried :+ (name -> argValue), closingContext)
          case Nil =>
            throw InvalidState(
              s"Tried to apply definition function ${PrintIR(function)} with no un-applied arguments (should not exist)"
            )

        }
      case function @ RTValue.ConstructorFunction(name, arguments, curried) =>
        arguments match {
          case _ :: Nil  => RTValue.ConstructorResult(name, curried :+ argValue)
          case _ :: tail => RTValue.ConstructorFunction(name, tail, curried :+ argValue)
          case Nil =>
            throw InvalidState(
              s"Tried to apply to constructor function ${PrintIR(function)} with no arguments (should not exist)"
            )

        }
      case nativeFunctionResult: RTValue.NativeFunctionResult =>
        val (arguments, curried, function) =
          nativeFunctionResult match {
            case RTValue.NativeFunction(arguments, curried, function) =>
              (arguments, curried, function)
            case RTValue.NativeInnerFunction(arguments, curried, function) =>
              (arguments, curried, function.injectEvaluator(this))
          }

        def assertCurriedNumArgs(num: Int) =
          if (curried.size != num) throw new WrongNumberOfArguments(nativeFunctionResult, num)
        // Once the uncurrying is done, we can call the function since we have all of the arguments available
        arguments match {
          case 1 =>
            function match {
              case NativeFunctionSignature.Fun1(f) =>
                assertCurriedNumArgs(0)
                f(argValue)
              case NativeFunctionSignature.Fun2(f) =>
                assertCurriedNumArgs(1)
                f(curried(0), argValue)
              case NativeFunctionSignature.Fun3(f) =>
                assertCurriedNumArgs(2)
                f(curried(0), curried(1), argValue)
              case NativeFunctionSignature.Fun4(f) =>
                assertCurriedNumArgs(3)
                f(curried(0), curried(1), curried(2), argValue)
              case NativeFunctionSignature.Fun5(f) =>
                assertCurriedNumArgs(4)
                f(curried(0), curried(1), curried(2), curried(3), argValue)
            }
          // If there are more arguments left in the native-signature, that needs we have more arguments to apply
          case x => RTValue.NativeFunction(x - 1, curried :+ argValue, function)
        }
      case other =>
        throw new UnexpectedType("Function", other, hint = "Expected because this was found in an Apply position")
    }

  def handleDestructure(
      va: ValueAttribs,
      node: RuntimeValue,
      pattern: Pattern[ValueAttribs],
      valueToDestruct: RuntimeValue,
      inValue: RuntimeValue,
      store: Store
  ): RTValue = {
    val value = loop(valueToDestruct, store)
    matchPatternCase(pattern, value) match {
      case None => throw UnmatchedPattern(value, node, pattern)
      case Some(bindings) =>
        loop(inValue, store.push(bindings.map { case (name, value) => name -> StoredValue.Eager(value) }))
    }
  }

  def handleConstructor(va: ValueAttribs, name: FQName): RTValue =
    globals.getCtor(name) match {
      case Some(SDKConstructor(List())) => RTValue.ConstructorResult(name, List())
      case Some(SDKConstructor(arguments)) =>
        RTValue.ConstructorFunction(name, arguments, List())
      case None =>
        val (pkg, mod, loc) = (name.getPackagePath, name.getModulePath, name.localName)
        throw new ConstructorNotFound(
          s"""Constructor mising from store:
             |pkg : $pkg
             |mod : $mod
             |loc : $loc
             |Store contents from that package:
             |  ${globals.ctors.keys.filter(_.getPackagePath == pkg).map(_.toString).mkString("\n\t")}
             |
             |Other Store Contents:
             |  ${globals.ctors.keys.map(_.toString).mkString("\n\t")}
             |""".stripMargin
        )
    }

  def handleField(
      va: ValueAttribs,
      recordValue: RuntimeValue,
      fieldName: Name,
      store: Store
  ): RTValue =
    loop(recordValue, store) match {
      case record @ RTValue.Record(fields) =>
        fields.getOrElse(
          fieldName,
          throw MissingField(record, fieldName)
        )
      case other => throw new UnexpectedType(
          "Record",
          other,
          hint = s"Expected because I tried to access .${fieldName.toCamelCase}"
        )
    }

  def handleFieldFunction(va: ValueAttribs, name: Name): RTValue = RTValue.FieldFunction(name)

  def handleIfThenElse(
      va: ValueAttribs,
      condition: RuntimeValue,
      thenValue: RuntimeValue,
      elseValue: RuntimeValue,
      store: Store
  ) =
    loop(condition, store) match {
      case RTValue.Primitive(true)  => loop(thenValue, store)
      case RTValue.Primitive(false) => loop(elseValue, store)
      case other => throw new UnexpectedType(
          "Boolean",
          other,
          hint = "Expected because I found this in the condition of an if statement"
        )
    }

  def handleLambda(
      va: ValueAttribs,
      pattern: Pattern[ValueAttribs],
      body: RuntimeValue,
      store: Store
  ): RTValue =
    RTValue.LambdaFunction(body, pattern, store.callStack)

  def handleLetDefinition(
      va: ValueAttribs,
      valueName: Name,
      valueDefinition: RuntimeDefinition,
      inValue: RuntimeValue,
      store: Store
  ) = {
    val value =
      if (valueDefinition.inputTypes.isEmpty) loop(valueDefinition.body, store)
      else
        RTValue.DefinitionFunction(valueDefinition.body, valueDefinition.inputTypes.toList, List(), store.callStack)
    loop(inValue, store.push(Map(valueName -> StoredValue.Eager(value))))
  }

  def handleLetRecursion(
      va: ValueAttribs,
      definitions: Map[Name, Definition[TypeAttribs, ValueAttribs]],
      inValue: RuntimeValue,
      store: Store
  ): RTValue = {
    val siblings = definitions.map { case (name, definition) =>
      name -> StoredValue.Lazy(definition, store.callStack, definitions)
    }
    loop(inValue, store.push(siblings))
  }

  def handleListValue(va: ValueAttribs, elements: List[RuntimeValue], store: Store): RTValue =
    RTValue.List(elements.map(loop(_, store)))

  def handlePatternMatch(
      va: ValueAttribs,
      node: RuntimeValue,
      value: RuntimeValue,
      cases: List[(Pattern[ValueAttribs], RuntimeValue)],
      store: Store
  ): RTValue = {
    val evaluated = loop(value, store)

    def firstPatternMatching(
        remainingCases: List[(Pattern[ValueAttribs], RuntimeValue)]
    ): (RuntimeValue, Map[Name, RTValue]) =
      remainingCases match {
        case (pattern, inValue) :: tail =>
          matchPatternCase(pattern, evaluated).map((inValue, _)).getOrElse(firstPatternMatching(tail))
        case Nil =>
          throw UnmatchedPattern(evaluated, node, cases.map(_._1): _*)
      }
    val (inValue, bindings) = firstPatternMatching(cases)
    loop(inValue, store.push(bindings.map { case (name, value) => name -> StoredValue.Eager(value) }))
  }

  def handleRecord(va: ValueAttribs, fields: List[(Name, RuntimeValue)], store: Store): RTValue =
    RTValue.Record(fields.map { case (name, value) => name -> loop(value, store) }.toMap)

  def handleReference(va: ValueAttribs, name: FQName, store: Store): RTValue =
    globals.getDefinition(name) match {
      case None =>
        val filtered = globals.definitions.keys.filter(_.getPackagePath == name.getPackagePath)
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
          RTValue.DefinitionFunction(
            valueDefinition.body,
            valueDefinition.inputTypes.toList,
            List(),
            store.callStack
          )
      case Some(SDKNativeValue(value)) => value
      case Some(SDKNativeFunction(function)) =>
        RTValue.NativeFunction(function.numArgs, List(), function)
      case Some(SDKNativeInnerFunction(function)) =>
        RTValue.NativeInnerFunction(function.numArgs, List(), function)
    }

  def handleTuple(va: ValueAttribs, elements: List[RuntimeValue], store: Store): RTValue = {
    val evaluatedElements = elements.map(loop(_, store))
    RTValue.Tuple(evaluatedElements)
  }

  def handleUnit(va: ValueAttribs): RTValue = RTValue.Unit()

  def handleUpdateRecord(
      va: ValueAttribs,
      valueToUpdate: RuntimeValue,
      fields: Map[Name, RuntimeValue],
      store: Store
  ): RTValue =
    loop(valueToUpdate, store) match {
      case RTValue.Record(oldFields) =>
        val newFields = fields.map { case (name, value) => name -> loop(value, store) }
        RTValue.Record(oldFields ++ newFields)
      case other =>
        throw UnexpectedType("Record", other, hint = "Expected because I found this in an update record node")
    }

  def handleVariable(va: ValueAttribs, name: Name, store: Store) =
    store.getVariable(name) match {
      case None                         => throw VariableNotFound(name)
      case Some(StoredValue.Eager(res)) => res
      case Some(StoredValue.Lazy(definition, parentContext, siblings)) =>
        val newBindings: Map[Name, StoredValue] = siblings.map { case (name, sibling) =>
          name -> StoredValue.Lazy(sibling, parentContext, siblings)
        }
        if (definition.inputTypes.isEmpty)
          loop(definition.body, store.push(newBindings))
        else
          RTValue.DefinitionFunction(
            definition.body,
            definition.inputTypes.toList,
            List(),
            store.push(newBindings).callStack
          )
    }
}
