package org.finos.morphir.testing.model

import org.finos.morphir.annotation._
import org.finos.morphir.naming._
import org.finos.morphir.mir._

@qualifiedModuleName(pkg"Morphir.SDK.Test" % mod"Model")
final case class Person(name: String, age: Int)
