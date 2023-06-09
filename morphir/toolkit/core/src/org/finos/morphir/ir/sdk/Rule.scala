package org.finos.morphir.ir.sdk

import zio.Chunk
import org.finos.morphir.ir.Module
import org.finos.morphir.ir.Module.QualifiedModuleName
import org.finos.morphir.ir.Type.Specification.TypeAliasSpecification
import org.finos.morphir.ir.Type._
import org.finos.morphir.ir.sdk.Basics.boolType
import org.finos.morphir.ir.sdk.Common._
import org.finos.morphir.ir.sdk.List.listType
import org.finos.morphir.ir.sdk.Maybe.maybeType
import org.finos.morphir.syntax.NamingSyntax._

object Rule {
  val moduleName: QualifiedModuleName = QualifiedModuleName.fromString("Rule")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Rule") -> TypeAliasSpecification(
        Chunk(name("a"), name("b")),
        tFun(tVar("a"))(maybeType(tVar("b")))
      ) ?? "Type that represents an rule."
    ),
    values = Map(
      vSpec("chain", "rules" -> listType(ruleType(tVar("a"), tVar("b"))))(ruleType(tVar("a"), tVar("b"))),
      vSpec("any", "value" -> tVar("a"))(boolType),
      vSpec("is", "ref" -> tVar("a"), "value" -> tVar("a"))(boolType),
      vSpec("anyOf", "ref" -> listType(tVar("a")), "value" -> tVar("a"))(boolType),
      vSpec("noneOf", "ref" -> listType(tVar("a")), "value" -> tVar("a"))(boolType)
    )
  )

  def ruleType(itemType1: UType, itemType2: UType): UType =
    reference(toFQName(moduleName, "Rule"), itemType1, itemType2)

  def ruleType[A](attributes: A)(itemType1: Type[A], itemType2: Type[A]): Type[A] =
    reference(attributes, toFQName(moduleName, "Rule"), itemType1, itemType2)
}
