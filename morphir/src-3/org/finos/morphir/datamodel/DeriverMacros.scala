package org.finos.morphir.datamodel

import org.finos.morphir.naming._
import org.finos.morphir.datamodel.Deriver.UnionType
import org.finos.morphir.naming.QualifiedModuleName

import scala.quoted.*
import scala.deriving.Mirror

trait GlobalDatamodelContext {
  def value: QualifiedModuleName
}

trait TypeDatamodelContext[T] {
  def value: QualifiedModuleName
  def nameOverride: Option[Name] = None
}

object DeriverMacros {
  import DeriverTypes._

  inline def typeName[T]: String = ${ typeNameImpl[T] }
  def typeNameImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].typeSymbol.name)
  }

  inline def summonNamespaceOrFail[T]: (QualifiedModuleName, Option[Name]) = ${ summonNamespaceOrFailImpl[T] }
  def summonNamespaceOrFailImpl[T: Type](using Quotes): Expr[(QualifiedModuleName, Option[Name])] = {
    import quotes.reflect._

    def summonTypeNamespace(): Option[Expr[(QualifiedModuleName, Option[Name])]] =
      Expr.summon[TypeDatamodelContext[T]].map(tns =>
        '{ ($tns.value, $tns.nameOverride) }
      )

    def summonGlobalNamespace(): Option[Expr[(QualifiedModuleName, Option[Name])]] =
      Expr.summon[GlobalDatamodelContext].map(gns =>
        '{ ($gns.value, None) }
      )

    val partialName: Expr[(QualifiedModuleName, Option[Name])] =
      summonTypeNamespace()
        .orElse(summonGlobalNamespace())
        .getOrElse {
          val tpeStr = TypeRepr.of[T].widen.show
          report.errorAndAbort(
            s"""
               |Cannot find a namespace for the type $tpeStr and a global default namespace has also not
               |been found. To define a namespace for a specific type do the following:
               |implicit val ns: TypeDatamodelContext[$tpeStr] = new TypeDatamodelContext[${TypeRepr.of[T].widen.show}] {
               |  import org.finos.morphir.datamodel.namespacing.*
               |  def value: Namespace = root / "path" / "to" / "my" / "package"
               |}
               |
               |To define a global namespace for all types do the following:
               |  implicit val ns: GlobalDatamodelContext = new GlobalDatamodelContext {
               |  def value: Namespace = root / "path" / "to" / "my" / "package"
               |}
               |""".stripMargin
          )
        }

    partialName
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

  inline def showType[T]: String = ${ showTypeImpl[T] }
  def showTypeImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(TypeRepr.of[T].simplified.typeSymbol.name)
  }

  inline def summonDeriver[T]: Deriver[T] = ${ summonDeriverImpl[T] }
  def summonDeriverImpl[T: Type](using Quotes): Expr[Deriver[T]] =
    import quotes.reflect._
    def failNotProductOrSum() =
      report.errorAndAbort(
        s"Cannot summon generic Deriver for the type (was not a Product or Sum): ${TypeRepr.of[T].widen.show} from `summonDeriver` (flags: ${TypeRepr.of[T].typeSymbol.flags.show}). Have you imported org.finos.morphir.datamodel.Derivers.{given, _}"
      )

    val specificDriver = Expr.summon[SpecificDeriver[T]]
    specificDriver match {
      case Some(value) => value
      case None =>
        val tpe   = TypeRepr.of[T]
        val flags = tpe.typeSymbol.flags
        if (Expr.summon[Mirror.ProductOf[T]].nonEmpty) {
          val genericDeriver = Expr.summon[Deriver[T]]
          genericDeriver match {
            case Some(value) => value
            case _ =>
              report.errorAndAbort(
                s"Cannot summon specific or generic Product Deriver for the product type: ${tpe.widen.show} from `summonDeriver` (flags: ${TypeRepr.of[T].typeSymbol.flags.show}). Have you imported org.finos.morphir.datamodel.Derivers.{given, _}"
              )
          }
        } else if (Expr.summon[Mirror.SumOf[T]].nonEmpty) {
          val genericDeriver = Expr.summon[Deriver[T]]
          genericDeriver match {
            case Some(value) => value
            case _ =>
              report.errorAndAbort(
                s"Cannot summon specific or generic Sum Deriver for the sum type: ${tpe.widen.show} from `summonDeriver` (flags: ${flags.show}). Have you imported org.finos.morphir.datamodel.Derivers.{given, _}"
              )
          }
        } else {
          failNotProductOrSum()
        }
    }

  inline def summonProductDeriver[T]: Deriver[T] = ${ summonProductDeriverImpl[T] }
  def summonProductDeriverImpl[T: Type](using Quotes): Expr[Deriver[T]] =
    import quotes.reflect._
    def failNotProduct() =
      report.errorAndAbort(
        s"Cannot summon generic Deriver for the type (was not a Product): ${TypeRepr.of[T].widen.show} from `summonProductDeriver` (flags: ${TypeRepr.of[T].typeSymbol.flags.show}). Have you imported org.finos.morphir.datamodel.Derivers.{given, _}"
      )
    val tpe   = TypeRepr.of[T]
    val flags = tpe.typeSymbol.flags

    // Even if it's a product-deriver, try to grab a specific deriver for it first because certain things
    // (e.g. the `::` class) should be as specific derivers (i.e. SpecificDeriver[List[T]]) instead of the generic one
    val specificDriver = Expr.summon[SpecificDeriver[T]]
    specificDriver match {
      case Some(value) => value
      case None =>
        if (Expr.summon[Mirror.ProductOf[T]].nonEmpty) {
          val genericDeriver = Expr.summon[Deriver[T]]
          genericDeriver match {
            case Some(value) => value
            case _ =>
              report.errorAndAbort(
                s"Cannot summon specific or generic Product Deriver for the product type: ${tpe.widen.show} from `summonProductDeriver` (flags: ${TypeRepr.of[T].typeSymbol.flags.show}). Have you imported org.finos.morphir.datamodel.Derivers.{given, _}"
              )
          }
        } else
          failNotProduct()
    }
}
