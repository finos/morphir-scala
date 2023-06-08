package org.finos.morphir.ir.sdk

import org.finos.morphir.ir.Module
import org.finos.morphir.ir.Module.QualifiedModuleName
import org.finos.morphir.ir.Type.Specification.OpaqueTypeSpecification
import org.finos.morphir.ir.Type._
import org.finos.morphir.ir.sdk.Basics.intType
import org.finos.morphir.ir.sdk.Common.{toFQName, vSpec}
import org.finos.morphir.ir.sdk.Maybe.maybeType
import org.finos.morphir.syntax.NamingSyntax._

object Int {

  val moduleName: QualifiedModuleName = QualifiedModuleName.fromString("Int")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Int8")  -> OpaqueTypeSpecification() ?? "Type that represents a 8-bit integer.",
      name("Int16") -> OpaqueTypeSpecification() ?? "Type that represents a 16-bit integer.",
      name("Int32") -> OpaqueTypeSpecification() ?? "Type that represents a 32-bit integer.",
      name("Int64") -> OpaqueTypeSpecification() ?? "Type that represents a 64-bit integer."
    ),
    values = Map(
      vSpec("fromInt8", "n" -> int8Type)(intType),
      vSpec("toInt8", "n" -> intType)(maybeType(int8Type)),
      vSpec("fromInt16", "n" -> int16Type)(intType),
      vSpec("toInt16", "n" -> intType)(maybeType(int16Type)),
      vSpec("fromInt32", "n" -> int32Type)(intType),
      vSpec("toInt32", "n" -> intType)(maybeType(int32Type)),
      vSpec("fromInt64", "n" -> int64Type)(intType),
      vSpec("toInt64", "n" -> intType)(maybeType(int64Type))
    )
  )

  lazy val int8Type: UType = reference(toFQName(moduleName, "Int8"))
  def int8Type[A](attributes: A): Type[A] =
    reference(attributes, toFQName(moduleName, "Int8"))

  lazy val int16Type: UType = reference(toFQName(moduleName, "Int16"))
  def int16Type[A](attributes: A): Type[A] =
    reference(attributes, toFQName(moduleName, "Int16"))

  lazy val int32Type: UType = reference(toFQName(moduleName, "Int32"))
  def int32Type[A](attributes: A): Type[A] =
    reference(attributes, toFQName(moduleName, "Int32"))

  lazy val int64Type: UType = reference(toFQName(moduleName, "Int64"))
  def int64Type[A](attributes: A): Type[A] =
    reference(attributes, toFQName(moduleName, "Int64"))

}
