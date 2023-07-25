package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.namespacing.PartialName

extension (v: Data.Boolean.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => Boolean
  ): SpecificDataEncoder[T] =
    new SpecificDataEncoder[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Boolean, partialName)
      override def encode(value: T): Data.Case = wrapper.construct(Data.Boolean(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Boolean): SpecificDataEncoder[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Boolean): SpecificDataEncoder[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Byte.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => Byte
  ): SpecificDataEncoder[T] =
    new SpecificDataEncoder[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Byte, partialName)
      override def encode(value: T): Data.Case = wrapper.construct(Data.Byte(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Byte): SpecificDataEncoder[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Byte): SpecificDataEncoder[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Decimal.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => scala.BigDecimal
  ): SpecificDataEncoder[T] =
    new SpecificDataEncoder[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Decimal, partialName)
      override def encode(value: T): Data.Case = wrapper.construct(Data.Decimal(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => scala.BigDecimal): SpecificDataEncoder[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => scala.BigDecimal): SpecificDataEncoder[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Integer.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => scala.BigInt
  ): SpecificDataEncoder[T] =
    new SpecificDataEncoder[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Integer, partialName)
      override def encode(value: T): Data.Case = wrapper.construct(Data.Integer(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => scala.BigInt): SpecificDataEncoder[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => scala.BigInt): SpecificDataEncoder[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Int16.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => Short
  ): SpecificDataEncoder[T] =
    new SpecificDataEncoder[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Int16, partialName)
      override def encode(value: T): Data.Case = wrapper.construct(Data.Int16(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Short): SpecificDataEncoder[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Short): SpecificDataEncoder[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Int32.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => Int
  ): SpecificDataEncoder[T] =
    new SpecificDataEncoder[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Int32, partialName)
      override def encode(value: T): Data.Case = wrapper.construct(Data.Int32(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Int): SpecificDataEncoder[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Int): SpecificDataEncoder[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.String.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => String
  ): SpecificDataEncoder[T] =
    new SpecificDataEncoder[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.String, partialName)
      override def encode(value: T): Data.Case = wrapper.construct(Data.String(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => String): SpecificDataEncoder[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => String): SpecificDataEncoder[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.LocalDate.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => java.time.LocalDate
  ): SpecificDataEncoder[T] =
    new SpecificDataEncoder[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.LocalDate, partialName)
      override def encode(value: T): Data.Case = wrapper.construct(Data.LocalDate(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => java.time.LocalDate): SpecificDataEncoder[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => java.time.LocalDate): SpecificDataEncoder[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Month.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => java.time.Month
  ): SpecificDataEncoder[T] =
    new SpecificDataEncoder[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Month, partialName)
      override def encode(value: T): Data.Case = wrapper.construct(Data.Month(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => java.time.Month): SpecificDataEncoder[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => java.time.Month): SpecificDataEncoder[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.LocalTime.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => java.time.LocalTime
  ): SpecificDataEncoder[T] =
    new SpecificDataEncoder[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.LocalTime, partialName)
      override def encode(value: T): Data.Case = wrapper.construct(Data.LocalTime(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => java.time.LocalTime): SpecificDataEncoder[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => java.time.LocalTime): SpecificDataEncoder[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Char.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String,
      fromScalaType: T => Char
  ): SpecificDataEncoder[T] =
    new SpecificDataEncoder[T] {
      val wrapper                              = SingleEnumWrapper(label, Concept.Char, partialName)
      override def encode(value: T): Data.Case = wrapper.construct(Data.Char(fromScalaType(value)))
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String, fromScalaType: T => Char): SpecificDataEncoder[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label, fromScalaType)
  }
  inline def deriveEnumWrapper[T](fromScalaType: T => Char): SpecificDataEncoder[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name, fromScalaType)
  }
}

extension (v: Data.Unit.type) {
  def deriveEnumWrapperNamespaced[T](
      partialName: PartialName,
      label: String
  ): SpecificDataEncoder[T] =
    new SpecificDataEncoder[T] {
      val wrapper                              = UnitEnumWrapper(label, partialName)
      override def encode(value: T): Data.Case = wrapper.construct
      override def concept: Concept            = wrapper.concept
    }
  inline def deriveEnumWrapper[T](label: String): SpecificDataEncoder[T] = {
    val (partialName, _) = DeriverMacros.summonNamespaceOrFail[T]
    deriveEnumWrapperNamespaced(partialName, label)
  }
  inline def deriveEnumWrapper[T]: SpecificDataEncoder[T] = {
    val name = DeriverMacros.typeName[T]
    deriveEnumWrapper(name)
  }
}
