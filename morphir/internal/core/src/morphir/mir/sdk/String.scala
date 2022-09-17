package org.finos.morphir.mir.sdk

import zio.Chunk
import org.finos.morphir.mir.Module.ModuleName
import org.finos.morphir.mir.Type.Specification.OpaqueTypeSpecification
import org.finos.morphir.mir.Type.Type._
import org.finos.morphir.mir.Type.{Type, UType}
import org.finos.morphir.mir.sdk.Basics.{boolType, floatType, intType}
import org.finos.morphir.mir.sdk.Char.charType
import org.finos.morphir.mir.sdk.Common._
import org.finos.morphir.mir.sdk.List.listType
import org.finos.morphir.mir.sdk.Maybe.maybeType
import org.finos.morphir.mir.{Module, NeedsAttributes}
import org.finos.morphir.syntax.NamingSyntax._

object String {
  val moduleName: ModuleName = ModuleName.fromString("String")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(name("String") -> OpaqueTypeSpecification() ?? "Type that represents a string of characters."),
    values = Map(
      vSpec("isEmpty", "s" -> stringType)(boolType),
      vSpec("length", "s" -> stringType)(intType),
      vSpec("reverse", "s" -> stringType)(stringType),
      vSpec("repeat", "n" -> intType, "s" -> stringType)(stringType),
      vSpec("replace", "match" -> stringType, "replacement" -> stringType, "s" -> stringType)(stringType),
      vSpec("append", "s1" -> stringType, "s2" -> stringType)(stringType),
      vSpec("concat", "list" -> listType(stringType))(stringType),
      vSpec("split", "sep" -> stringType, "s" -> stringType)(listType(stringType)),
      vSpec("join", "sep" -> stringType, "list" -> listType(stringType))(stringType),
      vSpec("words", "s" -> stringType)(listType(stringType)),
      vSpec("lines", "s" -> stringType)(listType(stringType)),
      vSpec("slice", "start" -> intType, "end" -> intType, "s" -> stringType)(stringType),
      vSpec("left", "n" -> intType, "s" -> stringType)(stringType),
      vSpec("right", "n" -> intType, "s" -> stringType)(stringType),
      vSpec("dropLeft", "n" -> intType, "s" -> stringType)(stringType),
      vSpec("dropRight", "n" -> intType, "s" -> stringType)(stringType),
      vSpec("contains", "ref" -> stringType, "s" -> stringType)(boolType),
      vSpec("startsWith", "ref" -> stringType, "s" -> stringType)(boolType),
      vSpec("endsWith", "ref" -> stringType, "s" -> stringType)(boolType),
      vSpec("indexes", "ref" -> stringType, "s" -> stringType)(listType(intType)),
      vSpec("indices", "ref" -> stringType, "s" -> stringType)(listType(intType)),
      vSpec("toInt", "s" -> stringType)(maybeType(intType)),
      vSpec("fromInt", "a" -> intType)(stringType),
      vSpec("toFloat", "s" -> stringType)(maybeType(floatType)),
      vSpec("fromFloat", "a" -> floatType)(stringType),
      vSpec("fromChar", "ch" -> charType)(stringType),
      vSpec("cons", "ch" -> charType, "s" -> stringType)(stringType),
      vSpec("uncons", "s" -> stringType)(maybeType(tuple(Chunk(charType, stringType)))),
      vSpec("toList", "s" -> stringType)(listType(charType)),
      vSpec("fromList", "a" -> listType(charType))(stringType),
      vSpec("toUpper", "s" -> stringType)(stringType),
      vSpec("toLower", "s" -> stringType)(stringType),
      vSpec("pad", "n" -> intType, "ch" -> charType, "s" -> stringType)(stringType),
      vSpec("padLeft", "n" -> intType, "ch" -> charType, "s" -> stringType)(stringType),
      vSpec("padRight", "n" -> intType, "ch" -> charType, "s" -> stringType)(stringType),
      vSpec("trim", "s" -> stringType)(stringType),
      vSpec("trimLeft", "s" -> stringType)(stringType),
      vSpec("trimRight", "s" -> stringType)(stringType),
      vSpec("map", "f" -> tFun(charType)(charType), "s" -> stringType)(stringType),
      vSpec("filter", "f" -> tFun(charType)(boolType), "s" -> stringType)(stringType),
      vSpec("foldl", "f" -> tFun(charType, tVar("b"))(tVar("b")), "z" -> tVar("b"), "s" -> stringType)(tVar("b")),
      vSpec("foldr", "f" -> tFun(charType, tVar("b"))(tVar("b")), "z" -> tVar("b"), "s" -> stringType)(tVar("b")),
      vSpec("any", "f" -> tFun(charType)(boolType), "s" -> stringType)(boolType),
      vSpec("all", "f" -> tFun(charType)(boolType), "s" -> stringType)(boolType)
    )
  )

  lazy val stringType: UType = reference(toFQName(moduleName, "String"))
  def stringType[A](attributes: A)(implicit ev: NeedsAttributes[A]): Type[A] =
    reference(attributes, toFQName(moduleName, "String"))

  // todo nativeFunctions
}
