package org.finos.morphir.ir.sdk

import org.finos.morphir.naming._
import zio.Chunk
import org.finos.morphir.ir.Type.Specification.OpaqueTypeSpecification
import org.finos.morphir.ir.Type.{reference => tRef, _}
import org.finos.morphir.ir.Value.{apply, reference, Value}
import org.finos.morphir.ir.sdk.Basics._
import org.finos.morphir.ir.sdk.List.listType
import org.finos.morphir.ir.sdk.Maybe.maybeType
import org.finos.morphir.ir.Module

object Dict extends MorphirIRSdkModule("Dict") {

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Dict") -> OpaqueTypeSpecification("k", "v") ?? "Type that represents a dictionary of key-value pairs."
    ),
    values = Map(
      vSpec("empty")(dictType(tVar("k"), tVar("v"))),
      vSpec("singleton", "key" -> tVar("comparable"), "value" -> tVar("v"))(dictType(tVar("comparable"), tVar("v"))),
      vSpec(
        "insert",
        "key"   -> tVar("comparable"),
        "value" -> tVar("v"),
        "dict"  -> dictType(tVar("comparable"), tVar("v"))
      )(dictType(tVar("comparable"), (tVar("v")))),
      vSpec(
        "update",
        "key"  -> tVar("comparable"),
        "f"    -> tFun(maybeType(tVar("v")))(maybeType(tVar("v"))),
        "dict" -> dictType(tVar("comparable"), tVar("v"))
      )(dictType(tVar("comparable"), tVar("v"))),
      vSpec("remove", "key" -> tVar("comparable"), "dict" -> dictType(tVar("comparable"), tVar("v")))(
        dictType(tVar("comparable"), tVar("v"))
      ),
      vSpec("isEmpty", "dict" -> dictType(tVar("comparable"), tVar("v")))(boolType),
      vSpec("member", "key" -> tVar("comparable"), "dict" -> dictType(tVar("comparable"), tVar("v")))(boolType),
      vSpec("get", "key" -> tVar("comparable"), "dict" -> dictType(tVar("comparable"), tVar("v")))(
        maybeType(tVar("v"))
      ),
      vSpec("size", "dict" -> dictType(tVar("comparable"), tVar("v")))(intType),
      vSpec("keys", "dict" -> dictType(tVar("k"), tVar("v")))(listType(tVar("k"))),
      vSpec("values", "dict" -> dictType(tVar("k"), tVar("v")))(listType(tVar("v"))),
      vSpec("toList", "dict" -> dictType(tVar("k"), tVar("v")))(listType(tuple(scala.List(tVar("k"), tVar("v"))))),
      vSpec("fromList", "list" -> listType(tuple(scala.List(tVar("comparable"), tVar("v")))))(
        dictType(tVar("comparable"), tVar("v"))
      ),
      vSpec("map", "f" -> tFun(tVar("k"), tVar("a"))(tVar("b")), "dict" -> dictType(tVar("k"), tVar("a")))(
        dictType(tVar("k"), tVar("b"))
      ),
      vSpec(
        "foldl",
        "f"    -> tFun(tVar("k"), tVar("v"), tVar("b"))(tVar("b")),
        "z"    -> tVar("b"),
        "list" -> dictType(tVar("k"), tVar("v"))
      )(tVar("b")),
      vSpec(
        "foldr",
        "f"    -> tFun(tVar("k"), tVar("v"), tVar("b"))(tVar("b")),
        "z"    -> tVar("b"),
        "list" -> dictType(tVar("k"), tVar("v"))
      )(tVar("b")),
      vSpec(
        "filter",
        "f"    -> tFun(tVar("comparable"), tVar("v"))(boolType),
        "dict" -> dictType(tVar("comparable"), tVar("v"))
      )(dictType(tVar("comparable"), tVar("v"))),
      vSpec(
        "partition",
        "f"    -> tFun(tVar("comparable"), tVar("v"))(boolType),
        "dict" -> dictType(tVar("comparable"), tVar("v"))
      )(tuple(scala.List(dictType(tVar("comparable"), tVar("v")), dictType(tVar("comparable"), tVar("v"))))),
      vSpec(
        "union",
        "dict1" -> dictType(tVar("comparable"), tVar("v")),
        "dict2" -> dictType(tVar("comparable"), tVar("v"))
      )(dictType(tVar("comparable"), tVar("v"))),
      vSpec(
        "intersect",
        "dict1" -> dictType(tVar("comparable"), tVar("v")),
        "dict2" -> dictType(tVar("comparable"), tVar("v"))
      )(dictType(tVar("comparable"), tVar("v"))),
      vSpec(
        "diff",
        "dict1" -> dictType(tVar("comparable"), tVar("v")),
        "dict2" -> dictType(tVar("comparable"), tVar("v"))
      )(dictType(tVar("comparable"), tVar("v"))),
      vSpec(
        "merge",
        "leftOnly"  -> tFun(tVar("comparable"), tVar("a"), tVar("result"))(tVar("result")),
        "both"      -> tFun(tVar("comparable"), tVar("a"), tVar("b"), tVar("result"))(tVar("result")),
        "rightOnly" -> tFun(tVar("comparable"), tVar("b"), tVar("result"))(tVar("result")),
        "dictLeft"  -> dictType(tVar("comparable"), tVar("a")),
        "dictRight" -> dictType(tVar("comparable"), tVar("b")),
        "input"     -> tVar("result")
      )(tVar("result"))
    )
  )

  def dictType(keyType: UType, valueType: UType): UType =
    tRef(fqn("Dict"), keyType, valueType)

  def dictType[A](attributes: A)(keyType: Type[A], valueType: Type[A]): Type[A] =
    tRef(attributes, fqn("dict"), keyType, valueType)

  def fromListValue[TA, VA](attributes: VA)(list: Value[TA, VA]): Value[TA, VA] =
    apply(
      attributes,
      reference(attributes, fqn("fromList")),
      list
    )

  def toListValue[TA, VA](attributes: VA)(list: Value[TA, VA]): Value[TA, VA] =
    apply(
      attributes,
      reference(attributes, fqn("toList")),
      list
    )
}
