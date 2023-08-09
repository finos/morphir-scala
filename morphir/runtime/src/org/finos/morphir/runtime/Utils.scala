package org.finos.morphir.runtime

import org.finos.morphir.naming.*

import org.finos.morphir.naming._
import org.finos.morphir.ir.{Type as T, Value as V}
import V.Value
import T.Type
import org.finos.morphir.ir.Field
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.ir.Type.UType
import zio.Chunk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.Value.{USpecification => UValueSpec, Definition => ValueDefinition}

class Distributions(dists : Map[PackageName, Distribution]){

  def lookupModuleSpecification(packageName: PackageName, module: ModuleName): Option[ModSpec.Raw] =
    dists.get(packageName) match {
      case Some(Library(_, _, packageDef)) =>
        packageDef.toSpecification.modules.get(module)
      case None => None
    }


  def lookupValueSpecification(
                                packageName: PackageName,
                                module: ModuleName,
                                localName: Name
                              ): Option[UValueSpec] =
    lookupModuleSpecification(packageName, module).flatMap(_.lookupValueSpecification(localName))

  def lookupTypeSpecification(pName: PackageName, module: ModuleName, localName: Name): Option[UTypeSpec] =
    lookupModuleSpecification(pName, module).flatMap(_.lookupTypeSpecification(localName))
}
object Distributions {
  def apply(dists: Distribution*): Distributions = {
    Distributions(dists.map {case (lib: Library) => lib.packageName -> lib}.toMap)
  }
}

object Extractors {
  object FQString {
    def unapply(fqName: FQName): Option[String] = Some(fqName.toString())
  }
  object ListRef {
    def unapply(tpe: UType): Option[UType] =
      tpe match {
        case Type.Reference(_, FQString("Morphir.SDK:List:list"), Chunk(elementType)) =>
          Some(elementType)
        case _ => None
      }
  }
  object MaybeRef {
    def unapply(tpe: UType): Option[UType] =
      tpe match {
        case Type.Reference(_, FQString("Morphir.SDK:Maybe:maybe"), Chunk(elementType)) =>
          Some(elementType)
        case _ => None
      }
  }
  object DictRef {
    def unapply(tpe: UType): Option[(UType, UType)] =
      tpe match {
        case Type.Reference(attributes, FQString("Morphir.SDK:Dict:dict"), Chunk(keyType, valType)) =>
          Some((keyType, valType))
        case _ => None
      }
  }
  trait CommonReference {
    val tpe: UType
    def unapply(argTpe: UType): Boolean =
      argTpe match {
        case Type.Reference(_, fqName, Chunk()) if fqName == tpe.asInstanceOf[Type.Reference[Unit]].typeName => true
        case _                                                                                               => false
      }
  }
  object IntRef extends CommonReference {
    final val tpe = Basics.intType
  }

  object Int32Ref extends CommonReference {
    final val tpe = sdk.Int.int32Type
  }
  object BoolRef extends CommonReference {
    final val tpe = Basics.boolType
  }
  object FloatRef extends CommonReference {
    final val tpe = Basics.floatType
  }
  object StringRef extends CommonReference {
    final val tpe = sdk.String.stringType
  }
  object CharRef extends CommonReference {
    final val tpe = sdk.Char.charType
  }
}

object Utils {
  import Extractors.*
  def applyBindings(tpe: UType, bindings: Map[Name, UType]): UType =
    tpe match {
      case Type.Variable(_, name) if bindings.contains(name) => bindings(name)
      case Type.Tuple(_, elements)                           => T.tupleVar(elements.map(applyBindings(_, bindings)): _*)
      case DictRef(keyType, valueType) =>
        sdk.Dict.dictType(applyBindings(keyType, bindings), applyBindings(valueType, bindings))
      case ListRef(elemType)  => sdk.List.listType(applyBindings(elemType, bindings))
      case MaybeRef(elemType) => sdk.Maybe.maybeType(applyBindings(elemType, bindings))
      case Type.Record(_, argFields) =>
        T.record(argFields.map(field => Field(field.name, applyBindings(field.data, bindings))))
      case Type.Function(_, argType, retType) =>
        T.function(applyBindings(argType, bindings), applyBindings(retType, bindings))
      case Type.Reference(_, name, argTypes) => T.reference(name, argTypes.map(applyBindings(_, bindings)))
      case other                             => other // leaf nodes
    }

  def typeCheckArg(arg: UType, param: UType, found: Map[Name, UType]): Either[TypeError, Map[Name, UType]] =
    (arg, param) match {
      case (argType, Type.Variable(_, name)) =>
        if (found.contains(name) && found(name) != argType) {
          Left(InferenceConflict(s"Both ${found(name)} and $argType bound to type variable $name"))
        } else {
          Right(found + (name -> argType))
        }
      case (Type.Unit(_), Type.Unit(_)) => Right(found)
      case (IntRef(), IntRef())         => Right(found) // Right?
      case (Int32Ref(), Int32Ref())     => Right(found)
      case (FloatRef(), FloatRef())     => Right(found)
      case (StringRef(), StringRef())   => Right(found)
      case (CharRef(), CharRef())       => Right(found)
      case (BoolRef(), BoolRef())       => Right(found)
      case (Type.Tuple(_, argElements), Type.Tuple(_, paramElements)) =>
        if (argElements.length != paramElements.length) {
          Left(new TypeMismatch(s"Different tuple arity between arg $argElements and parameter $paramElements"))
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
      case (ListRef(argElement), ListRef(paramElement))   => typeCheckArg(argElement, paramElement, found)
      case (MaybeRef(argElement), MaybeRef(paramElement)) => typeCheckArg(argElement, paramElement, found)
      case (Type.Record(_, argFields), Type.Record(_, paramFields)) =>
        if (argFields.length != paramFields.length) {
          Left(WrongRecordSize(s"Record lengths differ between arg : $argFields and param: $paramFields"))
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
      case (Type.ExtensibleRecord(_, _, _), Type.ExtensibleRecord(_, _, _)) =>
        Left(UnsupportedType(s"Extensible record type not supported (yet)"))
      case (Type.Reference(_, argTypeName, argTypeArgs), Type.Reference(_, paramTypeName, paramTypeArgs))
          if (argTypeName != paramTypeName) =>
        argTypeArgs.zip(paramTypeArgs).foldLeft(Right(found): Either[TypeError, Map[Name, UType]]) {
          case (acc, (argTpe, paramTpe)) =>
            acc.flatMap(found => typeCheckArg(argTpe, paramTpe, found))
        }
      case (otherArg, otherParam) => Left(NotImplementedType(s"Cannot match $otherArg with $otherParam"))
    }
  def specificationToType[TA](spec: V.Specification[TA]): Type[TA] =
    curryTypeFunction(spec.output, spec.inputs)

  def unCurryTypeFunction(
      curried: UType,
      args: List[UType],
      knownBindings: Map[Name, UType]
  ): RTAction[Any, TypeError, UType] =
    (curried, args) match {
      case (Type.Function(attributes, parameterType, returnType), head :: tail) =>
        for {
          bindings    <- RTAction.fromEither(typeCheckArg(head, parameterType, knownBindings))
          appliedType <- unCurryTypeFunction(returnType, tail, bindings)
        } yield appliedType
      case (tpe, Nil) => RTAction.succeed(applyBindings(tpe, knownBindings))
      case (nonFunction, head :: _) =>
        RTAction.fail(TooManyArgs(s"Tried to apply argument $head to non-function $nonFunction"))
    }
  // TODO: Implement
  def typeCheck[TA](t1: Type[TA], t2: Type[TA]): RTAction[Any, TypeError, Unit] = RTAction.succeed(())
  def curryTypeFunction[TA](inner: Type[TA], params: Chunk[(Name, Type[TA])]): Type[TA] =
    params match {
      case Chunk() => inner
      case chunk =>
        curryTypeFunction(Type.Function(getattributes(inner), chunk.head._2, inner), chunk.tail)
    }

  def getattributes[TA](tpe: Type[TA]): TA =
    tpe match {
      case Type.ExtensibleRecord(attributes, _, _) => attributes
      case Type.Function(attributes, _, _)         => attributes
      case Type.Record(attributes, _)              => attributes
      case Type.Reference(attributes, _, _)        => attributes // TODO: Ignored type arguments here might be an issue
      case Type.Tuple(attributes, _)               => attributes
      case Type.Unit(attributes)                   => attributes
      case Type.Variable(attributes, _)            => attributes
    }

}
