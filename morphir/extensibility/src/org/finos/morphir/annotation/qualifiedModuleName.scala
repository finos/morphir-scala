package org.finos.morphir.annotation
import scala.annotation._
import org.finos.morphir.naming._

final case class fullyQualifiedName(name: FQName)                     extends StaticAnnotation
final case class qualifiedModuleName(moduleName: QualifiedModuleName) extends StaticAnnotation
