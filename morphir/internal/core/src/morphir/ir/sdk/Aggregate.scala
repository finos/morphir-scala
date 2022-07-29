package morphir.mir.sdk

import morphir.mir.Module
import morphir.mir.Module.ModuleName
import morphir.mir.Type.Specification.OpaqueTypeSpecification
import morphir.mir.Type.Type._
import morphir.mir.Type.{Type, UType}
import morphir.mir.sdk.Basics.{boolType, floatType}
import morphir.mir.sdk.Common._
import morphir.mir.sdk.List.listType
import morphir.syntax.NamingSyntax._

object Aggregate {

  val moduleName: ModuleName = ModuleName.fromString("Aggregate")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(name("Aggregation") -> OpaqueTypeSpecification("a", "key") ?? ""),
    values = Map(
      vSpec("count")(aggregationType(tVar("a"), unit)),
      vSpec("sumOf", "getValue" -> tFun(tVar("a"))(floatType))(aggregationType(tVar("a"), unit)),
      vSpec("averageOf", "getValue" -> tFun(tVar("a"))(floatType))(aggregationType(tVar("a"), unit)),
      vSpec("minimumOf", "getValue" -> tFun(tVar("a"))(floatType))(aggregationType(tVar("a"), unit)),
      vSpec("maximumOf", "getValue" -> tFun(tVar("a"))(floatType))(aggregationType(tVar("a"), unit)),
      vSpec("weightedAverageOf", "getWeight" -> tFun(tVar("a"))(floatType), "getValue" -> tFun(tVar("a"))(floatType))(
        aggregationType(tVar("a"), unit)
      ),
      vSpec("byKey", "key" -> tFun(tVar("a"))(tVar("key")), "agg" -> aggregationType(tVar("a"), unit))(
        aggregationType(tVar("a"), tVar("key"))
      ),
      vSpec("withFilter", "filter" -> tFun(tVar("a"))(boolType), ("agg" -> aggregationType(tVar("a"), tVar("key"))))(
        aggregationType(tVar("a"), tVar("key"))
      ),
      vSpec(
        "aggregateMap",
        "agg1" -> aggregationType(tVar("a"), tVar("key1")),
        "f"    -> tFun(floatType, tVar("a"))(tVar("b")),
        "list" -> listType(tVar("a"))
      )(listType(tVar("b"))),
      vSpec(
        "aggregateMap2",
        "agg1" -> aggregationType(tVar("a"), tVar("key1")),
        "agg2" -> aggregationType(tVar("a"), tVar("key2")),
        "f"    -> tFun(floatType, floatType, tVar("a"))(tVar("b")),
        "list" -> listType(tVar("a"))
      )(listType(tVar("b"))),
      vSpec(
        "aggregateMap3",
        "agg1" -> aggregationType(tVar("a"), tVar("key1")),
        "agg2" -> aggregationType(tVar("a"), tVar("key2")),
        "agg3" -> aggregationType(tVar("a"), tVar("key3")),
        "f"    -> tFun(floatType, floatType, floatType, tVar("a"))(tVar("b")),
        "list" -> listType(tVar("a"))
      )(listType(tVar("b")))
    )
  )

  def aggregationType(aType: UType, keyType: UType): UType =
    reference(toFQName(moduleName, "Aggregation"), aType, keyType)

  def aggregationType[A](attributes: A)(aType: Type[A], keyType: Type[A]): Type[A] =
    reference(attributes, toFQName(moduleName, "Aggregation"), aType, keyType)
}
