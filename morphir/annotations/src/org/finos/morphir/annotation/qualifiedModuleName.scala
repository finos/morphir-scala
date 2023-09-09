package org.finos.morphir.annotation
import scala.annotation._

/// Annotation used to define th package name for a type or module in Morphir.
/// NOTE: That a package in Morphir is not a synonym for a namespace like it is in Java or Scala, but it represents a unit of distribution
/// like a Nuget, NPM, or Maven package.
final case class packageName(value: String) extends StaticAnnotation

final case class fullyQualifiedName(packageName: String, moduleName: String, localName: String) extends StaticAnnotation
final case class qualifiedModuleName(packageName: String, moduleName: String)                   extends StaticAnnotation
