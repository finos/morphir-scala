package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.namespacing.PartialName

extension (v: Data.Boolean.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => Boolean
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Boolean, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Boolean(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Boolean): SpecificDeriver[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Boolean): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Byte.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => Byte
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Byte, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Byte(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Byte): SpecificDeriver[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Byte): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Decimal.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => scala.BigDecimal
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Decimal, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Decimal(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => scala.BigDecimal): SpecificDeriver[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => scala.BigDecimal): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Integer.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => scala.BigInt
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Integer, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Integer(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => scala.BigInt): SpecificDeriver[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => scala.BigInt): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}
