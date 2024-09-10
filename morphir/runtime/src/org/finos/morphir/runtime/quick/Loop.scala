package org.finos.morphir.runtime.quick

import org.finos.morphir.naming.*
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Value.{Pattern, Value, TypedValue, TypedDefinition}
import org.finos.morphir.ir.Value.Value.{List as ListValue, *}
import Helpers.{listToTuple, matchPatternCase, unpackLit}
import org.finos.morphir.runtime.SDKValue.{SDKNativeFunction, SDKNativeInnerFunction, SDKNativeValue}
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.runtime.internal.{NativeFunctionSignature, StoredValue}
import org.finos.morphir.runtime.internal.InvokeableEvaluator
import org.finos.morphir.ir.printing.{DetailLevel, PrintIR}
import org.finos.morphir.runtime.{RTValue, SDKConstructor, SDKValue, Utils}
import org.finos.morphir.runtime.MorphirRuntimeError.*
import org.finos.morphir.runtime.CodeLocation
import org.finos.morphir.util.PrintRTValue

private[morphir] case class Loop(globals: GlobalDefs) extends InvokeableEvaluator {
  def loop(ir: TypedValue, store: Store, codeLocation: CodeLocation): RTValue =
    try (LoopFrame(globals, codeLocation).loop(ir, store))
    catch {
      case m: EvaluationError => throw m.stack(codeLocation)
      case e: Throwable       => throw new ExternalError(e).stack(codeLocation)
    }

  def handleApplyResult5(
      va: UType,
      functionValue: RTValue,
      arg1: RTValue,
      arg2: RTValue,
      arg3: RTValue,
      arg4: RTValue,
      arg5: RTValue
  ): RTValue = {
    val partiallyAppliedFunction =
      handleApplyResult(va, functionValue, arg1)
    val partiallyAppliedFunction2 =
      handleApplyResult(va, partiallyAppliedFunction, arg2)
    val partiallyAppliedFunction3 =
      handleApplyResult(va, partiallyAppliedFunction2, arg3)
    val partiallyAppliedFunction4 =
      handleApplyResult(va, partiallyAppliedFunction3, arg4)
    val result =
      handleApplyResult(va, partiallyAppliedFunction4, arg5)
    result
  }

  def handleApplyResult4(
      va: UType,
      functionValue: RTValue,
      arg1: RTValue,
      arg2: RTValue,
      arg3: RTValue,
      arg4: RTValue
  ): RTValue = {
    val partiallyAppliedFunction =
      handleApplyResult(va, functionValue, arg1)
    val partiallyAppliedFunction2 =
      handleApplyResult(va, partiallyAppliedFunction, arg2)
    val partiallyAppliedFunction3 =
      handleApplyResult(va, partiallyAppliedFunction2, arg3)
    val result =
      handleApplyResult(va, partiallyAppliedFunction3, arg4)
    result
  }

  def handleApplyResult3(
      va: UType,
      functionValue: RTValue,
      arg1: RTValue,
      arg2: RTValue,
      arg3: RTValue
  ): RTValue = {
    val partiallyAppliedFunction =
      handleApplyResult(va, functionValue, arg1)
    val partiallyAppliedFunction2 =
      handleApplyResult(va, partiallyAppliedFunction, arg2)
    val result =
      handleApplyResult(va, partiallyAppliedFunction2, arg3)
    result
  }

  def handleApplyResult2(
      va: UType,
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
      va: UType,
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
      case RTValue.LambdaFunction(body, pattern, context, loc) =>
        val newBindings = matchPatternCase(pattern, argValue)
          .getOrElse(throw UnmatchedPattern(
            argValue,
            functionValue,
            location = Some(loc),
            pattern
          ))
          .map { case (name, value) => name -> StoredValue.Eager(value) }
        loop(body, Store(context.push(newBindings)), loc)
      case function @ RTValue.DefinitionFunction(body, arguments, curried, closingContext, loc) =>
        arguments match {
          case (name, _, _) :: Nil =>
            val newBindings = (curried :+ (name -> argValue)).map { case (name, value) =>
              name -> StoredValue.Eager(value)
            }.toMap
            loop(body, Store(closingContext.push(newBindings)), loc)
          case (name, _, _) :: tail =>
            RTValue.DefinitionFunction(body, tail, curried :+ (name -> argValue), closingContext, loc)
          case Nil =>
            throw InvalidState(
              "Tried to apply definition function with no un-applied arguments (should not exist)",
              location = Some(loc),
              function
            )

        }
      case function @ RTValue.ConstructorFunction(name, arguments, curried) =>
        arguments match {
          case _ :: Nil  => RTValue.ConstructorResult(name, curried :+ argValue)
          case _ :: tail => RTValue.ConstructorFunction(name, tail, curried :+ argValue)
          case Nil =>
            throw InvalidState(
              "Tried to apply to constructor function with no arguments (should not exist)",
              location = None,
              function
            )

        }
      case function @ RTValue.ImplicitConstructorFunction(name, fields, curried) =>
        fields match {
          case head :: Nil  => RTValue.Record(curried ++ Map(head.name -> argValue))
          case head :: tail => RTValue.ImplicitConstructorFunction(name, tail, curried ++ Map(head.name -> argValue))
          case Nil =>
            throw InvalidState(
              "Tried to apply to implicit constructor function with no arguments (should not exist)",
              location = None,
              function
            )

        }
      case nativeFunctionResult: RTValue.NativeFunctionResult =>
        val (arguments, curried, function, loc) =
          nativeFunctionResult match {
            case RTValue.NativeFunction(arguments, curried, function, loc) =>
              (arguments, curried, function, loc)
            case RTValue.NativeInnerFunction(arguments, curried, function, loc) =>
              (arguments, curried, function.injectEvaluator(this), loc)
          }

        try {
          def assertCurriedNumArgs(num: Int) =
            if (curried.size != num) throw WrongNumberOfArguments(nativeFunctionResult, num, location = Some(loc))
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
                case NativeFunctionSignature.Fun6(f) =>
                  assertCurriedNumArgs(5)
                  f(curried(0), curried(1), curried(2), curried(3), curried(4), argValue)
              }
            // If there are more arguments left in the native-signature, that needs we have more arguments to apply
            case x => RTValue.NativeFunction(x - 1, curried :+ argValue, function, loc)
          }
        } catch {
          case e: EvaluationError => throw e.stack(loc)
          case e: Throwable       => throw ExternalError(e, location = Some(loc)).stack(loc)
        }
      case other =>
        throw UnexpectedType("Function", other, hint = "Expected because this was found in an Apply position")
    }
}

/**
 * Helper class for handling recursive evaluation over a single function call (i.e., within one CodeLocation)
 */
private[morphir] case class LoopFrame(globals: GlobalDefs, codeLocation: CodeLocation) {
  def loop(ir: TypedValue, store: Store): RTValue =
    try
      ir match {
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
    catch {
      case e: EvaluationError => throw e.source(ir.toString)
      case e: Throwable       => throw ExternalError(e, location = Some(codeLocation)).source(ir.toString)
    }

  def handleLiteral(va: UType, literal: Lit) = unpackLit(literal)

  def handleApply(
      va: UType,
      function: TypedValue,
      argument: TypedValue,
      store: Store
  ): RTValue = {
    val functionValue = loop(function, store)
    val argValue      = loop(argument, store)
    // New call, so we go to back to "Loop":
    Loop(globals).handleApplyResult(va, functionValue, argValue)
  }

  def handleDestructure(
      va: UType,
      node: TypedValue,
      pattern: Pattern[UType],
      valueToDestruct: TypedValue,
      inValue: TypedValue,
      store: Store
  ): RTValue = {
    val value = loop(valueToDestruct, store)
    matchPatternCase(pattern, value) match {
      case None => throw UnmatchedPattern(value, node, location = Some(codeLocation), pattern)
      case Some(bindings) =>
        loop(inValue, store.push(bindings.map { case (name, value) => name -> StoredValue.Eager(value) }))
    }
  }

  def handleConstructor(va: UType, name: FQName): RTValue =
    globals.getCtor(name) match {
      case Some(SDKConstructor.Explicit(List())) => RTValue.ConstructorResult(name, List())
      case Some(SDKConstructor.Explicit(arguments)) =>
        RTValue.ConstructorFunction(name, arguments, List())
      case Some(SDKConstructor.Implicit(List())) => RTValue.Record(Map())
      case Some(SDKConstructor.Implicit(fields)) => RTValue.ImplicitConstructorFunction(name, fields, Map())
      case None =>
        val (pkg, mod, loc) = (name.getPackagePath, name.getModulePath, name.localName)
        throw ConstructorNotFound(
          s"""Constructor mising from store:
             |pkg : $pkg
             |mod : $mod
             |loc : $loc
             |Store contents from that package:
             |  ${globals.ctors.keys.filter(_.getPackagePath == pkg).map(_.toString).mkString("\n\t")}
             |
             |Other Store Contents:
             |  ${globals.ctors.keys.map(_.toString).mkString("\n\t")}
             |""".stripMargin,
          location = Some(codeLocation)
        )
    }

  def handleField(
      va: UType,
      recordValue: TypedValue,
      fieldName: Name,
      store: Store
  ): RTValue =
    loop(recordValue, store) match {
      case record @ RTValue.Record(fields) =>
        fields.getOrElse(
          fieldName,
          throw MissingField(record, fieldName, location = Some(codeLocation))
        )
      case other => throw UnexpectedType(
          "Record",
          other,
          hint = s"Expected because I tried to access .${fieldName.toCamelCase}",
          location = Some(codeLocation)
        )
    }

  def handleFieldFunction(va: UType, name: Name): RTValue = RTValue.FieldFunction(name)

  def handleIfThenElse(
      va: UType,
      condition: TypedValue,
      thenValue: TypedValue,
      elseValue: TypedValue,
      store: Store
  ) =
    loop(condition, store) match {
      case RTValue.Primitive(true)  => loop(thenValue, store)
      case RTValue.Primitive(false) => loop(elseValue, store)
      case other => throw UnexpectedType(
          "Boolean",
          other,
          hint = "Expected because I found this in the condition of an if statement",
          location = Some(codeLocation)
        )
    }

  def handleLambda(
      va: UType,
      pattern: Pattern[UType],
      body: TypedValue,
      store: Store
  ): RTValue =
    RTValue.LambdaFunction(body, pattern, store.callStack, CodeLocation.AnonymousFunction(codeLocation))

  def handleLetDefinition(
      va: UType,
      valueName: Name,
      valueDefinition: TypedDefinition,
      inValue: TypedValue,
      store: Store
  ) = {
    val value =
      if (valueDefinition.inputTypes.isEmpty) loop(valueDefinition.body, store)
      else
        RTValue.DefinitionFunction(
          valueDefinition.body,
          valueDefinition.inputTypes.toList,
          List(),
          store.callStack,
          CodeLocation.AnonymousFunction(codeLocation)
        )
    loop(inValue, store.push(Map(valueName -> StoredValue.Eager(value))))
  }

  def handleLetRecursion(
      va: UType,
      definitions: Map[Name, TypedDefinition],
      inValue: TypedValue,
      store: Store
  ): RTValue = {
    val siblings = definitions.map { case (name, definition) =>
      name -> StoredValue.Lazy(definition, store.callStack, definitions)
    }
    loop(inValue, store.push(siblings))
  }

  def handleListValue(va: UType, elements: List[TypedValue], store: Store): RTValue =
    RTValue.List(elements.map(loop(_, store)))

  def handlePatternMatch(
      va: UType,
      node: TypedValue,
      value: TypedValue,
      cases: List[(Pattern[UType], TypedValue)],
      store: Store
  ): RTValue = {
    val evaluated = loop(value, store)

    def firstPatternMatching(
        remainingCases: List[(Pattern[UType], TypedValue)]
    ): (TypedValue, Map[Name, RTValue]) =
      remainingCases match {
        case (pattern, inValue) :: tail =>
          matchPatternCase(pattern, evaluated).map((inValue, _)).getOrElse(firstPatternMatching(tail))
        case Nil =>
          throw UnmatchedPattern(evaluated, node, location = Some(codeLocation), cases.map(_._1): _*)
      }
    val (inValue, bindings) = firstPatternMatching(cases)
    loop(inValue, store.push(bindings.map { case (name, value) => name -> StoredValue.Eager(value) }))
  }

  def handleRecord(va: UType, fields: List[(Name, TypedValue)], store: Store): RTValue =
    RTValue.Record(fields.map { case (name, value) => name -> loop(value, store) }.toMap)

  def handleReference(va: UType, name: FQName, store: Store): RTValue =
    globals.getDefinition(name) match {
      case None =>
        val filtered = globals.definitions.keys.filter(_.getPackagePath == name.getPackagePath)
        val hint = if (Utils.isNative(name)) "You might be calling an unimplemented native function"
        else "You might be calling a function not defined in the given distributions"
        throw DefinitionNotFound(
          s"""name $name not found in store.
             | Hint: $hint
             | For that package, store contains:
             | \t${filtered.map(_.toString).mkString("\n\t")}""".stripMargin,
          location = Some(codeLocation)
        )
      case Some(SDKValue.SDKValueDefinition(valueDefinition)) =>
        if (valueDefinition.inputTypes.isEmpty) {
          loop(valueDefinition.body, store)
        } else
          RTValue.DefinitionFunction(
            valueDefinition.body,
            valueDefinition.inputTypes.toList,
            List(),
            store.callStack,
            CodeLocation.TopLevelFunction(name)
          )
      case Some(SDKNativeValue(value)) => value
      case Some(SDKNativeFunction(function)) =>
        RTValue.NativeFunction(function.numArgs, List(), function, CodeLocation.NativeFunction(name))
      case Some(SDKNativeInnerFunction(function)) =>
        RTValue.NativeInnerFunction(function.numArgs, List(), function, CodeLocation.NativeFunction(name))
    }

  def handleTuple(va: UType, elements: List[TypedValue], store: Store): RTValue = {
    val evaluatedElements = elements.map(loop(_, store))
    RTValue.Tuple(evaluatedElements)
  }

  def handleUnit(va: UType): RTValue = RTValue.Unit()

  def handleUpdateRecord(
      va: UType,
      valueToUpdate: TypedValue,
      fields: Map[Name, TypedValue],
      store: Store
  ): RTValue =
    loop(valueToUpdate, store) match {
      case RTValue.Record(oldFields) =>
        val newFields = fields.map { case (name, value) => name -> loop(value, store) }
        RTValue.Record(oldFields ++ newFields)
      case other =>
        throw UnexpectedType(
          "Record",
          other,
          hint = "Expected because I found this in an update record node",
          location = Some(codeLocation)
        )
    }

  def handleVariable(va: UType, name: Name, store: Store) =
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
            store.push(newBindings).callStack,
            CodeLocation.AnonymousFunction(codeLocation)
          )
    }
}
