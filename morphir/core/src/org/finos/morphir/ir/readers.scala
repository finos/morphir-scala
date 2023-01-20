package org.finos
package morphir
package ir

import org.finos.morphir.ir.Name.Name
import org.finos.morphir.ir.QName.QName
import org.finos.morphir.ir.FQName.FQName
import org.finos.morphir.ir.Module.{ModuleName, ModulePath}
import org.finos.morphir.ir.Path.Path
import org.finos.morphir.ir.Package.PackageName
import scala.collection.mutable
import upickle.core.{Annotator, ArrVisitor, Visitor}

trait IRReaders

trait IRValueReaders extends IRTypeReaders { self: Annotator => }
trait IRTypeReaders  extends NamingReaders { self: Annotator => }
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
