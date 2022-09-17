package org.finos.morphir.mir.sdk

import zio.Chunk
import org.finos.morphir.mir.Module
import org.finos.morphir.mir.Module.ModuleName
import org.finos.morphir.mir.Type.Specification.TypeAliasSpecification
import org.finos.morphir.mir.Type.Type._
import org.finos.morphir.mir.Type.{Type, UType}
import org.finos.morphir.mir.sdk.Basics.boolType
import org.finos.morphir.mir.sdk.Common._
import org.finos.morphir.mir.sdk.List.listType
import org.finos.morphir.mir.sdk.Maybe.maybeType
import org.finos.morphir.syntax.NamingSyntax._

object Rule {
  val moduleName: ModuleName = ModuleName.fromString("Rule")

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
