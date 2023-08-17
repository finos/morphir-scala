package org.finos.morphir.datamodel

import scala.deriving.*
import org.finos.morphir.datamodel.SpecificDeriver

extension [A](da: SpecificDeriver[A])
  inline def aliasedAs(alias: String) =
    new SpecificDeriver[A] {
      override def derive(value: A): Data =
        Data.Aliased(da.derive(value), this.concept)
      override def concept: Concept.Alias = {
        val (pname, _, _) = DeriverMacros.summonNamespaceOrFail
        Concept.Alias(pname % alias, da.concept)
      }
    }

extension (r: Data.Record.type) {
  inline def withImplicitFQN(name: String, fieldsRaw: (String, Data)*): Data.Record = {
    val (pname, _, _) = DeriverMacros.summonNamespaceOrFail
    val fields        = fieldsRaw.toList.map { case (labelString, data) => (Label(labelString), data) }
    Data.Record(pname % name, fields)
  }
}

extension (r: SingleEnumWrapper.type) {
  inline def withImplicitFQN(label: String, shape: Concept): SingleEnumWrapper = {
    val (pname, _, _) = DeriverMacros.summonNamespaceOrFail
    SingleEnumWrapper(label, shape, pname)
  }
}

// Implicit deriver instances have been moved org.finos.morphir.datamodel.package object.

implicit inline def autoDeriver[T]: Deriver[T] = Deriver.gen[T]
