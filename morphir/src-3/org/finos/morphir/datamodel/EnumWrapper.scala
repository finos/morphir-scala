package org.finos.morphir.datamodel

import org.finos.morphir.naming._

extension (v: Data.Boolean.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => Boolean
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Boolean, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Boolean(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Boolean): SpecificDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Boolean): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Byte.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => Byte
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Byte, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Byte(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Byte): SpecificDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Byte): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Decimal.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => scala.BigDecimal
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Decimal, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Decimal(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => scala.BigDecimal): SpecificDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => scala.BigDecimal): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Integer.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => scala.BigInt
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Integer, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Integer(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => scala.BigInt): SpecificDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => scala.BigInt): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Int16.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => Short
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Int16, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Int16(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Short): SpecificDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Short): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Int32.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => Int
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Int32, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Int32(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Int): SpecificDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Int): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.String.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => String
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.String, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.String(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => String): SpecificDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => String): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.LocalDate.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => java.time.LocalDate
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.LocalDate, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.LocalDate(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => java.time.LocalDate): SpecificDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => java.time.LocalDate): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Month.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => java.time.Month
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Month, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Month(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => java.time.Month): SpecificDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => java.time.Month): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.LocalTime.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => java.time.LocalTime
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.LocalTime, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.LocalTime(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => java.time.LocalTime): SpecificDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => java.time.LocalTime): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Char.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => Char
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Char, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Char(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Char): SpecificDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Char): SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Unit.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String
  ): SpecificDeriver[T] =
    new SpecificDeriver[T] {
      val wrapper                              = UnitEnumWrapper(label, partialName)
      override def derive(value: T): Data.Case = wrapper.construct
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String): SpecificDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label)
  }
  inline def deriveEnumWrapper[T]: SpecificDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name)
  }
}
