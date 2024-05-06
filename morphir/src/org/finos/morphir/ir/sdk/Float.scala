
package org.finos.morphir.ir.sdk

import org.finos.morphir.ir.Module
import org.finos.morphir.ir.Type.Specification.OpaqueTypeSpecification
import org.finos.morphir.ir.Type._
import org.finos.morphir.ir.sdk.Basics._
import org.finos.morphir.ir.sdk.Common._
import org.finos.morphir.ir.sdk.Maybe.maybeType
import org.finos.morphir.ir.sdk.String.stringType
import org.finos.morphir.syntax.NamingSyntax._

object Float extends MorphirIRSdkModule("Float") {

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Float") -> OpaqueTypeSpecification() ?? "Type that represents a Float."
    ),
    values = Map(
      vSpec("fromInt", "n" -> intType)(floatType),
      vSpec("round", "f" -> floatType)(intType),
      vSpec("floor", "f" -> floatType)(intType),
      vSpec("ceiling", "f" -> floatType)(intType),
      vSpec("truncate", "f" -> floatType)(intType)
    )
  )

  lazy val decimalType: UType =
    reference(toFQName("Float"))
  def decimalType[A](attributes: A): Type[A] =
    reference(attributes, fqn("Float"))

  }
