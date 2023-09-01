package org.finos.morphir.testing.model

import org.finos.morphir.annotation._
import org.finos.morphir.naming._
import org.finos.morphir.mir._

@qualifiedModuleName("Morphir.SDK.Test", "Model")
final case class Person(name: String, age: Int)

@fullyQualifiedName("Morphir.SDK.Testing", "Test.Models", "Employee")
final case class Employee(name: String, age: Int, salary: Int)
