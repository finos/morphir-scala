package org.finos.morphir.mir.sdk

import zio.Chunk
import org.finos.morphir.mir.Module.ModuleName
import org.finos.morphir.mir.Type.Specification.OpaqueTypeSpecification
import org.finos.morphir.mir.Type.Type._
import org.finos.morphir.mir.Type.{Type, UType}
import org.finos.morphir.mir.Value.Value
import org.finos.morphir.mir.Value.Value.{Apply, Reference}
import org.finos.morphir.mir.sdk.Basics._
import org.finos.morphir.mir.sdk.Common._
import org.finos.morphir.mir.sdk.List.listType
import org.finos.morphir.mir.sdk.Maybe.maybeType
import org.finos.morphir.mir.{FQName, Module, Name, Path}
import org.finos.morphir.syntax.NamingSyntax._

object Dict {
  val moduleName: ModuleName = ModuleName.fromString("Dict")

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
      vSpec("toList", "dict" -> dictType(tVar("k"), tVar("v")))(listType(tuple(Chunk(tVar("k"), tVar("v"))))),
      vSpec("fromList", "list" -> listType(tuple(Chunk(tVar("comparable"), tVar("v")))))(
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
      )(tuple(Chunk(dictType(tVar("comparable"), tVar("v")), dictType(tVar("comparable"), tVar("v"))))),
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
    reference(toFQName(moduleName, "dict"), keyType, valueType)

  def dictType[A](attributes: A)(keyType: Type[A], valueType: Type[A]): Type[A] =
    reference(attributes, toFQName(moduleName, "dict"), keyType, valueType)

  def fromListValue[TA, VA](attributes: VA)(list: Value[TA, VA]): Value[TA, VA] =
    Apply(
      attributes,
      Reference(attributes, FQName(Path("morphir", "s", "d", "k"), Path("dict"), Name("from", "list"))),
      list
    )

  def toListValue[TA, VA](attributes: VA)(list: Value[TA, VA]): Value[TA, VA] =
    Apply(
      attributes,
      Reference(attributes, FQName(Path("morphir", "s", "d", "k"), Path("dict"), Name("to", "list"))),
      list
    )
}
