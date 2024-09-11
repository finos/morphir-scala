package org.finos.morphir.runtime

import org.finos.morphir.naming.*
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Value.{Pattern, TypedValue, Value, USpecification as UValueSpec}
import org.finos.morphir.ir.Type.{Field, Type, UType, USpecification as UTypeSpec}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.ir.Module.Specification as ModSpec
import zio.Chunk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.Value.{Definition as ValueDefinition, USpecification as UValueSpec}
import org.finos.morphir.ir.Type.USpecification as UTypeSpec
import org.finos.morphir.ir.printing.{DetailLevel, PrintIR}
import org.finos.morphir.runtime.MorphirRuntimeError.*
import TypeError.*
import org.finos.morphir.runtime.CodeLocation.TopLevelFunction

object Utils {
  import Extractors.Types.*

  // Recurses over a type tree and applies known bindings (i.e., replaces variables with their bound type)
  def applyBindings(tpe: UType, bindings: Map[Name, UType]): UType =
    tpe match {
      case l @ LeafType()                                    => l
      case Type.Variable(_, name) if bindings.contains(name) => bindings(name)
      case Type.Variable(_, name)  => T.variable(name) // Not an error - may be unbound in this context
      case Type.Tuple(_, elements) => T.tupleVar(elements.map(applyBindings(_, bindings)): _*)
      case DictRef(keyType, valueType) =>
        sdk.Dict.dictType(applyBindings(keyType, bindings), applyBindings(valueType, bindings))
      case ListRef(elemType)  => sdk.List.listType(applyBindings(elemType, bindings))
      case MaybeRef(elemType) => sdk.Maybe.maybeType(applyBindings(elemType, bindings))
      case Type.Record(_, argFields) =>
        T.record(argFields.map(field => Field(field.name, applyBindings(field.data, bindings))))
      case Type.ExtensibleRecord(_, name, argFields) =>
        T.extensibleRecord(name, argFields.map(field => Field(field.name, applyBindings(field.data, bindings))))
      case Type.Function(_, argType, retType) =>
        T.function(applyBindings(argType, bindings), applyBindings(retType, bindings))
      case Type.Reference(_, name, argTypes) => T.reference(name, argTypes.map(applyBindings(_, bindings)))
      case Type.Unit(_)                      => T.unit
    }

  // Checks an argument against a paramter, and returns and inferable generic types from the pair (or an error)
  // TODO: Move to type checker, or remove altogether
  def typeCheckArg(arg: UType, param: UType, found: Map[Name, UType])(
      implicit options: RTExecutionContext.Options
  ): Either[TypeError, Map[Name, UType]] = {
    def failIfChecked(error: TypeError): Either[TypeError, Map[Name, UType]] =
      options.enableTyper match {
        case EnableTyper.Enabled =>
          Left(error)
        case EnableTyper.Warn =>
          println(s"[WARNING] ${error.message}")
          Right(found)
        case EnableTyper.Disabled =>
          Right(found)
      }
    (arg, param) match {
      case (argType, Type.Variable(_, name)) =>
        if (found.contains(name) && found(name) != argType) {
          failIfChecked(
            InferenceConflict(found(name), argType, name)
          )
        } else {
          Right(found + (name -> argType))
        }
      case (l @ LeafType(), r @ LeafType()) if l == r => Right(found)
      case (Type.Tuple(_, argElements), Type.Tuple(_, paramElements)) =>
        if (argElements.length != paramElements.length) {
          failIfChecked(new SizeMismatch(
            argElements.length,
            paramElements.length,
            s"Different tuple arity between arg ${PrintIR(argElements)} and parameter ${PrintIR(paramElements)}"
          ))
        } else {
          argElements.zip(paramElements).foldLeft(Right(found): Either[TypeError, Map[Name, UType]]) {
            case (acc, (argElement, paramElement)) =>
              acc.flatMap(found => typeCheckArg(argElement, paramElement, found))
          }
        }
      case (DictRef(argKey, argValue), DictRef(paramKey, paramValue)) =>
        for {
          keyBindings   <- typeCheckArg(argKey, paramKey, found)
          valueBindings <- typeCheckArg(argValue, paramValue, keyBindings)
        } yield valueBindings
      case (ResultRef(argErr, argOk), ResultRef(paramErr, paramOk)) =>
        for {
          errBindings <- typeCheckArg(argErr, paramErr, found)
          okBindings  <- typeCheckArg(argOk, paramOk, errBindings)
        } yield okBindings
      case (ListRef(argElement), ListRef(paramElement))   => typeCheckArg(argElement, paramElement, found)
      case (MaybeRef(argElement), MaybeRef(paramElement)) => typeCheckArg(argElement, paramElement, found)
      case (Type.Record(_, argFields), Type.Record(_, paramFields)) =>
        if (argFields.length != paramFields.length) {
          failIfChecked(new SizeMismatch(
            argFields.length,
            paramFields.length,
            s"Record lengths differ between arg : ${PrintIR(argFields)} and param: ${PrintIR(paramFields)}"
          ))
        } else {
          argFields.zip(paramFields).foldLeft(Right(found): Either[TypeError, Map[Name, UType]]) {
            case (acc, (argField, paramField)) =>
              acc.flatMap(found => typeCheckArg(argField.data, paramField.data, found))
          }
        }
      case (Type.Function(_, argArg, argReturn), Type.Function(_, paramArg, paramReturn)) =>
        for {
          argBindings   <- typeCheckArg(argArg, paramArg, found)
          paramBindings <- typeCheckArg(argReturn, paramReturn, argBindings)
        } yield paramBindings
      case (first @ Type.ExtensibleRecord(_, _, _), Type.ExtensibleRecord(_, _, _)) =>
        failIfChecked(new TypeError.UnsupportedType(first, hint = s"Extensible record type not supported (yet)"))
      case (Type.Reference(_, argTypeName, argTypeArgs), Type.Reference(_, paramTypeName, paramTypeArgs))
          if (argTypeName == paramTypeName) =>
        argTypeArgs.zip(paramTypeArgs).foldLeft(Right(found): Either[TypeError, Map[Name, UType]]) {
          case (acc, (argTpe, paramTpe)) =>
            acc.flatMap(found => typeCheckArg(argTpe, paramTpe, found))
        }
      case (otherArg, otherParam) =>
        failIfChecked(UnknownTypeMismatch(otherArg, otherParam, "Could not match arg to param on entry point"))
    }
  }

  def specificationToType(spec: UValueSpec): UType =
    curryTypeFunction(spec.output, spec.inputs)

  def findTypeBindings(
      curried: UType,
      args: List[TypedValue],
      dists: Distributions,
      knownBindings: Map[Name, UType]
  )(implicit options: RTExecutionContext.Options): RTAction[Any, TypeError, UType] = {
    val dealiaser = new Dealiased(dists)
    (curried, args) match {
      case (Type.Function(_, parameterType, returnType), head :: tail) =>
        for {
          bindings    <- RTAction.fromEither(typeCheckArg(head.attributes, parameterType, knownBindings))
          appliedType <- findTypeBindings(returnType, tail, dists, bindings)
        } yield appliedType
      case (tpe, Nil) => RTAction.succeed(applyBindings(tpe, knownBindings))
      case (dealiaser(inner), args) =>
        findTypeBindings(inner, args, dists, knownBindings)
      case (nonFunction, head :: _) =>
        RTAction.fail(new ImproperType(nonFunction, s"Tried to apply argument ${PrintIR(head)} to non-function"))
    }
  }
  def isNative(fqn: FQName): Boolean = {
    val example = FQName.fromString("Morphir.SDK:Basics:equal")
    fqn.getPackagePath == example.getPackagePath
  }

  def fqnToCodeLocation(fqn: FQName): CodeLocation =
    if (isNative(fqn)) {
      CodeLocation.NativeFunction(fqn)
    } else {
      CodeLocation.TopLevelFunction(fqn)
    }

  def curryTypeFunction(inner: UType, params: Chunk[(Name, UType)]): UType =
    params match {
      case Chunk() => inner
      case chunk =>
        T.function(chunk.head._2, curryTypeFunction(inner, chunk.tail))
    }
}
