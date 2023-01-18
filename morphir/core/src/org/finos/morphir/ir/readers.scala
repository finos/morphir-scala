package org.finos
package morphir
package ir

import scala.collection.mutable
import upickle.core.{Annotator, ArrVisitor, Visitor}

trait IRReaders

trait IRValueReaders extends IRTypeReaders { self: Annotator => }
trait IRTypeReaders  extends NamingReaders { self: Annotator => }
trait NamingReaders extends upickle.implicits.Readers { self: Annotator =>
  implicit val NameReader: Reader[Name.Name] = implicitly[Reader[List[String]]].map(Name.fromList)
}
