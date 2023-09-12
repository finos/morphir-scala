package org.finos.morphir.ir.sdk

import org.finos.morphir.naming._
import org.finos.morphir.ir.Module
import org.finos.morphir.ir.Type.Specification.OpaqueTypeSpecification
import org.finos.morphir.ir.Type._
import org.finos.morphir.ir.sdk.Basics.{boolType, intType}

object Char extends MorphirIRSdkModule("Char") {

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Char") -> OpaqueTypeSpecification() ?? "Type that represents a single character."
    ),
    values = Map(
      vSpec("isUpper", "c" -> charType)(boolType),
      vSpec("isLower", "c" -> charType)(boolType),
      vSpec("isAlpha", "c" -> charType)(boolType),
      vSpec("isAlphaNum", "c" -> charType)(boolType),
      vSpec("isDigit", "c" -> charType)(boolType),
      vSpec("isOctDigit", "c" -> charType)(boolType),
      vSpec("isHexDigit", "c" -> charType)(boolType),
      vSpec("toUpper", "c" -> charType)(charType),
      vSpec("toLower", "c" -> charType)(charType),
      vSpec("toLocaleUpper", "c" -> charType)(charType),
      vSpec("toLocaleLower", "c" -> charType)(charType),
      vSpec("toCode", "c" -> charType)(intType),
      vSpec("fromCode", ("c" -> intType))(charType)
    )
  )

  lazy val charType: UType = reference(fqn("Char"))
  def charType[A](attributes: A): Type[A] =
    reference(attributes, fqn("Char"))
}
