package org.finos.morphir.datamodel

import org.finos.morphir.naming._

extension (v: Data.Boolean.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => Boolean
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Boolean, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Boolean(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Boolean): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Boolean): CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Byte.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => Byte
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Byte, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Byte(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Byte): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Byte): CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Decimal.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => scala.BigDecimal
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Decimal, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Decimal(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => scala.BigDecimal): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => scala.BigDecimal): CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Integer.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => scala.BigInt
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Integer, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Integer(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => scala.BigInt): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => scala.BigInt): CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Int16.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => Short
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Int16, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Int16(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Short): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Short): CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Int32.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => Int
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Int32, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Int32(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Int): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Int): CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Int64.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => Long
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Int64, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Int64(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Long): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Long): CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.String.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => String
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.String, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.String(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => String): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => String): CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.LocalDate.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => java.time.LocalDate
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.LocalDate, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.LocalDate(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => java.time.LocalDate): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => java.time.LocalDate): CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Month.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => java.time.Month
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Month, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Month(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => java.time.Month): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => java.time.Month): CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.LocalTime.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => java.time.LocalTime
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.LocalTime, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.LocalTime(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => java.time.LocalTime): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => java.time.LocalTime): CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Char.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String,
      fromScalaType: T => Char
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Char, partialName)
      override def derive(value: T): Data.Case = wrapper.construct(Data.Char(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Char): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Char): CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Unit.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: QualifiedModuleName,
      label: String
  ): CustomDeriver[T] =
    new CustomDeriver[T] {
      val wrapper                              = UnitEnumWrapper(label, partialName)
      override def derive(value: T): Data.Case = wrapper.construct
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String): CustomDeriver[T] = {
    val (partialName, _, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label)
  }
  inline def deriveEnumWrapper[T]: CustomDeriver[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name)
  }
}
