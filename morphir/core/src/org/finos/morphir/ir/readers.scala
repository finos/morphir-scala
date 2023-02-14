package org.finos
package morphir
package ir

import org.finos.morphir.ir.Name.Name
import org.finos.morphir.ir.QName.QName
import org.finos.morphir.ir.FQName.FQName
import org.finos.morphir.ir.Module.{ModuleName, ModulePath}
import org.finos.morphir.ir.Path.Path
import org.finos.morphir.ir.Package.PackageName
import org.finos.morphir.ir.Type.Field
import org.finos.morphir.ir.Type.Type
import scala.collection.mutable
import upickle.core.{Abort, Annotator, ArrVisitor, Visitor}
import upickle.default.{read, write, Reader, ReadWriter}
import upickle.default.{ReadWriter => RW, macroRW, macroR}
import upickle.default._
import upickle._

trait IRReaders

trait IRValueReaders extends IRTypeReaders { self: Annotator => }

trait IRTypeReaders extends NamingReaders { self: Annotator =>

  implicit def FieldTypeReader[A: Reader]: Reader[Field[A]] = macroR

  implicit def ExtensibleRecordTypeReader[A: Reader]: Reader[Type.ExtensibleRecord[A]] =
    Tuple4Reader[String, A, Name, List[Field[A]]].map {
      case ("ExtensibleRecord", attributes, name, fields) =>
        Type.ExtensibleRecord(attributes, name, fields)
      case (other, attributes, name, fields) =>
        throw Abort(
          s"Expected ExtensibleRecord, got $other with attributes: $attributes, name: $name and fields: $fields"
        )
    }

  implicit def FunctionTypeReader[A: Reader]: Reader[Type.Function[A]] =
    Tuple4Reader[String, A, Type[A], Type[A]].map {
      case ("Function", attributes, argumentType, returnType) =>
        Type.Function(attributes, argumentType, returnType)
      case (other, attributes, argumentType, returnType) =>
        throw Abort(
          s"Expected Function, got $other with attributes: $attributes, argumentType: $argumentType and returnType: $returnType"
        )
    }

  implicit def RecordTypeReader[A: Reader]: Reader[Type.Record[A]] =
    Tuple3Reader[String, A, List[Field[A]]].map {
      case ("ExtensibleRecord", attributes, fields) =>
        Type.Record(attributes, fields)
      case (other, attributes, fields) =>
        throw Abort(
          s"Expected Record, got $other with attributes: $attributes, and fields: $fields"
        )
    }

  implicit def ReferenceTypeReader[A: Reader]: Reader[Type.Reference[A]] =
    Tuple4Reader[String, A, FQName, List[Type[A]]].map {
      case ("Reference", attributes, typeName, typeParams) =>
        Type.Reference(attributes, typeName, typeParams)
      case (other, attributes, typeName, typeParams) =>
        throw Abort(
          s"Expected Reference, got $other with attributes: $attributes, typeName: $typeName and typeParams: $typeParams"
        )
    }

  implicit def TupleTypeReader[A: Reader]: Reader[Type.Tuple[A]] =
    Tuple3Reader[String, A, List[Type[A]]].map {
      case ("Tuple", attributes, elements) =>
        Type.Tuple(attributes, elements)
      case (other, attributes, elements) =>
        throw Abort(
          s"Expected Tuple, got $other with attributes: $attributes and elements: $elements"
        )
    }

  implicit def UnitTypeReader[A: Reader]: Reader[Type.Unit[A]] =
    Tuple2Reader[String, A].map {
      case ("Unit", attributes) =>
        Type.Unit(attributes)
      case (other, attributes) =>
        throw Abort(
          s"Expected Unit, got $other with attributes: $attributes"
        )
    }

  implicit def VariableTypeReader[A: Reader]: Reader[Type.Variable[A]] =
    Tuple3Reader[String, A, Name].map {
      case ("Variable", attributes, name) =>
        Type.Variable(attributes, name)
      case (other, attributes, name) =>
        throw Abort(
          s"Expected Variable, got $other with attributes: $attributes and name: $name"
        )
    }

  implicit def TypeReader[A: Reader]: Reader[Type[A]] = ???
  // Reader.merge(
  //   // ExtensibleRecordTypeReader[A],
  //   // FunctionTypeReader[A],
  //   // RecordTypeReader[A],
  //   // ReferenceTypeReader[A],
  //   // TupleTypeReader[A],
  //   // UnitTypeReader[A],
  //   VariableTypeReader[A]
  // )
}

trait NamingReaders extends upickle.implicits.Readers { self: Annotator =>
  implicit val NameReader: Reader[Name]               = implicitly[Reader[List[String]]].map(Name.fromList)
  implicit val PathReader: Reader[Path]               = implicitly[Reader[List[Name]]].map(Path.fromList)
  implicit val ModulePathReader: Reader[ModulePath]   = PathReader.map(ModulePath(_))
  implicit val PackageNameReader: Reader[PackageName] = PathReader.map(PackageName(_))

  implicit val QNameReader: Reader[QName] = Tuple2Reader[Path, Name].map(QName.fromTuple)
  implicit val FQNameReader: Reader[FQName] = Tuple3Reader[PackageName, ModulePath, Name].map {
    case (packagePath, modulePath, localName) => FQName(packagePath, modulePath, localName)
  }
  implicit val ModuleNameReader: Reader[ModuleName] = Tuple2Reader[Path, Name].map { case (namespace, localName) =>
    ModuleName(namespace, localName)
  }
}
