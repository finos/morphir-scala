package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import zio.test.Gen

trait QNameGen {
  final def qName[R](modulePathGen: Gen[R, Path], localNameGen: Gen[R, Name]): Gen[R, QName] =
    for {
      modulePath <- modulePathGen
      localName  <- localNameGen
    } yield QName(modulePath, localName)

  final val qName: Gen[Any, QName] = qName(PathGen.path, NameGen.name)
}

object QNameGen extends QNameGen
