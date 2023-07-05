package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.Deriver.UnionType

import scala.quoted.*
import scala.reflect.ClassTag

object DeriverMacros {
  import DeriverTypes._

  inline def typeName[T]: String = ${ typeNameImpl[T] }
  def typeNameImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].typeSymbol.name)
  }

  inline def showFlags[T]: String = ${ showFlagsImpl[T] }
  def showFlagsImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].typeSymbol.flags.show)
  }

  private def flagsOf[T: Type](using Quotes): quotes.reflect.Flags = {
    import quotes.reflect._
    TypeRepr.of[T].typeSymbol.flags
  }

  inline def inferUnionType[T]: UnionType = ${ inferUnionTypeImpl[T] }
  def inferUnionTypeImpl[T: Type](using Quotes): Expr[UnionType] = {
    import quotes.reflect._
    val flags = flagsOf[T]
    if (flags.is(Flags.Sealed & Flags.Trait))
      '{ UnionType.SealedTrait }
    else if (flags.is(Flags.Enum))
      '{ UnionType.Enum }
    else report.errorAndAbort(
      s"Type ${TypeRepr.of[T].show} is not a sealed trait or enum and Unions are not supported yet"
    )
  }

  inline def isEnum[T]: Boolean = ${ isEnumImpl[T] }
  def isEnumImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect._
    Expr(flagsOf[T].is(Flags.Enum) && !(TypeRepr.of[T] <:< TypeRepr.of[List[Any]]))
  }

  inline def isEnumOrSealedTrait[T]: Boolean = ${ isEnumOrSealedTraitImpl[T] }
  def isEnumOrSealedTraitImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect._
    val isEnum        = flagsOf[T].is(Flags.Enum)
    val isSealedTrait = flagsOf[T].is(Flags.Sealed) && flagsOf[T].is(Flags.Trait)
    val result        = (isEnum || isSealedTrait) && !(TypeRepr.of[T] <:< TypeRepr.of[List[Any]])
    Expr(result)
  }

  inline def isSealedTrait[T]: Boolean = ${ isSealedTraitImpl[T] }
  def isSealedTraitImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect._
    Expr(flagsOf[T].is(Flags.Sealed & Flags.Trait) && !(TypeRepr.of[T] <:< TypeRepr.of[List[Any]]))
  }

  inline def errorOnType[T](msg: String): Nothing = ${ errorOnType[T]('msg) }
  def errorOnType[T: Type](msg: Expr[String])(using Quotes): Expr[Nothing] = {
    import quotes.reflect._
    val msgConst =
      msg match {
        case Expr(str: String) => str
        case _                 => report.errorAndAbort(s"Error-on-type has a non-constant value: ${msg.show}")
      }
    report.errorAndAbort(s"$msgConst: ${TypeRepr.of[T].widen.show}")
  }

  inline def isCaseClass[T]: Boolean = ${ isCaseClassImpl[T] }
  def isCaseClassImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect._
    val flags = flagsOf[T]
    // for some reason case objects are considered case classes (or at least have a case flag so make sure it's not a module)
    // also, for some reason in Scala 3, the List.:: instance is actually a case class!
    Expr(flags.is(Flags.Case) && !flags.is(Flags.Module) && !(TypeRepr.of[T] <:< TypeRepr.of[List[Any]]))
  }

  inline def summonClassTagOrFail[T]: ClassTag[T] = ${ summonClassTagOrFailImpl[T] }
  def summonClassTagOrFailImpl[T: Type](using Quotes): Expr[ClassTag[T]] = {
    import quotes.reflect._
    Expr.summon[ClassTag[T]] match {
      case Some(value) => value
      case None =>
        report.errorAndAbort(s"A classTag for the type ${TypeRepr.of[T].show} could not be found!")
    }
  }

  inline def showType[T]: String = ${ showTypeImpl[T] }
  def showTypeImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].simplified.typeSymbol.name)
  }

  inline def summonDeriver[T]: Deriver[T] = ${ summonDeriverImpl[T] }
  def summonDeriverImpl[T: Type](using Quotes): Expr[Deriver[T]] =
    import quotes.reflect._
    def failNotProduct() =
      report.errorAndAbort(
        s"Cannot summon generic Deriver for the type (was not a Product): ${TypeRepr.of[T].widen.show} (flags: ${TypeRepr.of[T].typeSymbol.flags.show}). Have you imported org.finos.morphir.datamodel.Derivers.{given, _}"
      )

    val specificDriver = Expr.summon[SpecificDeriver[T]]
    specificDriver match {
      case Some(value) => value
      case None =>
        if (!TypeRepr.of[T].typeSymbol.flags.is(Flags.Module))
          Type.of[T] match {
            case '[IsProduct[p]] =>
              val genericDeriver = Expr.summon[GenericProductDeriver[p]]
              genericDeriver match {
                case Some(value) => '{ $value.asInstanceOf[Deriver[T]] }
                case _ =>
                  report.errorAndAbort(
                    s"Cannot summon specific or generic Deriver for the type: ${TypeRepr.of[T].widen.show}. Have you imported org.finos.morphir.datamodel.Derivers.{given, _}"
                  )
              }
            case _ => failNotProduct()
          }
        else
          failNotProduct()
    }

  inline def summonProductDeriver[T]: Deriver[T] = ${ summonProductDeriverImpl[T] }
  def summonProductDeriverImpl[T: Type](using Quotes): Expr[Deriver[T]] =
    import quotes.reflect._
    def failNotProduct() =
      report.errorAndAbort(
        s"Cannot summon generic Deriver for the type (was not a Product): ${TypeRepr.of[T].widen.show} (flags: ${TypeRepr.of[T].typeSymbol.flags.show}). Have you imported org.finos.morphir.datamodel.Derivers.{given, _}"
      )
    // Not sure why but in Scala 3 case-objects are products. No idea why but we shouldn't be treating one of those
    // as a product so if it's a case-object (i.e. Module) don't consider it to be a product
    // (that's what the extra check is for!)
    if (!TypeRepr.of[T].typeSymbol.flags.is(Flags.Module))
      Type.of[T] match {
        case '[IsProduct[p]] =>
          val genericDeriver = Expr.summon[GenericProductDeriver[p]]
          genericDeriver match {
            case Some(value) => '{ $value.asInstanceOf[Deriver[T]] }
            case _ =>
              report.errorAndAbort(
                s"Cannot summon generic Product-Deriver for the Product type: ${TypeRepr.of[T].simplified.widen.show} (flags: ${TypeRepr.of[T].typeSymbol.flags.show}). Have you imported org.finos.morphir.datamodel.Derivers.{given, _}"
              )
          }
        case _ => failNotProduct()
      }
    else
      failNotProduct()
}
