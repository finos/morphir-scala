package org.finos.morphir.ir.sdk

import zio.Chunk
import org.finos.morphir.ir.Type.Specification.CustomTypeSpecification
import org.finos.morphir.ir.Type._
import org.finos.morphir.ir.Value.{apply, constructor, RawValue, Value}
import org.finos.morphir.ir.{Module, NeedsAttributes}
import scala.annotation.unused

object Maybe extends MorphirIRSdkModule("Maybe") {
  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Maybe") -> CustomTypeSpecification(
        Chunk(name("a")),
        UConstructors(
          Map(name("Just") -> Chunk((name("value"), variable(name("a")))), name("Nothing") -> Chunk.empty)
        )
      ) ?? "Type that represents an optional value."
    ),
    values = Map(
      vSpec("andThen", "f" -> tFun(tVar("a"))(maybeType(tVar("b"))), "maybe" -> maybeType(tVar("a")))(
        maybeType(tVar("b"))
      ),
      vSpec("map", "f" -> tFun(tVar("a"))(tVar("b")), "maybe" -> maybeType(tVar("a")))(maybeType(tVar("b"))),
      vSpec(
        "map2",
        "f"      -> tFun(tVar("a"), tVar("b"))(tVar("r")),
        "maybe1" -> maybeType(tVar("a")),
        "maybe2" -> maybeType(tVar("b"))
      )(maybeType(tVar("r"))),
      vSpec(
        "map3",
        "f"      -> tFun(tVar("a"), tVar("b"), tVar("c"))(tVar("r")),
        "maybe1" -> maybeType(tVar("a")),
        "maybe2" -> maybeType(tVar("b")),
        "maybe3" -> maybeType(tVar("c"))
      )(maybeType(tVar("r"))),
      vSpec(
        "map4",
        "f"      -> tFun(tVar("a"), tVar("b"), tVar("c"), tVar("d"))(tVar("r")),
        "maybe1" -> maybeType(tVar("a")),
        "maybe2" -> maybeType(tVar("b")),
        "maybe3" -> maybeType(tVar("c")),
        "maybe4" -> maybeType(tVar("d"))
      )(maybeType(tVar("r"))),
      vSpec(
        "map5",
        "f"      -> tFun(tVar("a"), tVar("b"), tVar("c"), tVar("d"), tVar("e"))(tVar("r")),
        "maybe1" -> maybeType(tVar("a")),
        "maybe2" -> maybeType(tVar("b")),
        "maybe3" -> maybeType(tVar("c")),
        "maybe4" -> maybeType(tVar("d")),
        "maybe5" -> maybeType(tVar("e"))
      )(maybeType(tVar("r"))),
      vSpec("withDefault", "default" -> tVar("a"), "maybe" -> maybeType(tVar("a")))(tVar("a"))
    )
  )

  def maybeType(itemType: UType): UType =
    reference(fqn("Maybe"), itemType)

  def maybeType[A](attributes: A, itemType: Type[A])(implicit @unused ev: NeedsAttributes[A]): Type[A] =
    reference(attributes, fqn("Maybe"), itemType)

  def just(value: RawValue): RawValue =
    apply(constructor(fqn("Just")), value)

  def just[VA](va: VA)(value: Value[Nothing, VA])(implicit @unused ev: NeedsAttributes[VA]): Value[Nothing, VA] =
    apply(va, constructor(va, fqn("Just")), value)

  lazy val nothing: RawValue =
    constructor(fqn("Nothing"))
  def nothing[VA](va: VA)(implicit @unused ev: NeedsAttributes[VA]): Value[Nothing, VA] =
    constructor(va, fqn("Nothing"))

  // todo add nativeFunctions
}
