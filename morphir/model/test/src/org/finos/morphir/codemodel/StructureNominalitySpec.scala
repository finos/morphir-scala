package org.finos.morphir.codemodel

import kyo.test.*
import kyo.{Chunk, Result, Schema, Structure}

/**
 * Spike R3: can `kyo.Structure.Value` faithfully hold a Morphir record as an untyped value tree, and is it purely
 * structural (carrying no nominal type identity), as decisions D2/D3 assume?
 *
 * D2/D3 put Morphir's nominal type identity in `codemodel.Type` (`Reference(attrs, fqName, args)`) and claim
 * `Structure.Value` carries data only. This spike checks both halves: field-name fidelity through encode/decode, the
 * shape `Structure.of[A]` reports, and — the actual question — whether `Structure.Value` retains anything that would
 * let a consumer recover which Scala type produced it.
 *
 * `Structure.Type.Product` (unlike `Structure.Value.Record`) *does* carry a `name: String` and a `tag: Tag[Any]`, both
 * derived from the Scala class, not from a Morphir `FQName`. That name lives on the `Type` tier only. The tests below
 * confirm the `Value` tier drops it: two unrelated case classes with the same field shape encode to structurally
 * identical trees and decode interchangeably.
 */
class StructureNominalitySpec extends Test[Any]:

  // Two case classes with an identical field shape but unrelated names, used to probe whether
  // Structure.Value carries anything that would block decoding one type's encoding as the other.
  final case class Customer(id: Int, name: String) derives Schema
  final case class Widget(id: Int, name: String) derives Schema

  // A case class whose field *names* differ from Customer's, same field count and primitive
  // kinds, used to confirm decode is driven by field name, not position.
  final case class Wrong(sku: Int, label: String) derives Schema

  private val ada = Customer(1, "Ada")

  "encodes a record with field names and values intact" in {
    val dynamic = Structure.encode(ada)
    dynamic match
      case Structure.Value.Record(fields) =>
        assert(fields == Chunk(("id", Structure.Value.Integer(1)), ("name", Structure.Value.Str("Ada"))))
      case other =>
        assert(false, s"expected a Record, got $other")
  }

  "decodes a Record back to the original typed value" in {
    val dynamic = Structure.encode(ada)
    assert(Structure.decode[Customer](dynamic) == Result.succeed(ada))
  }

  "Structure.of[A] reports a Product with the declared field names and primitive kinds" in {
    val tpe = Structure.of[Customer]
    tpe match
      case p: Structure.Type.Product =>
        assert(p.name == "Customer")
        assert(p.fields.map(_.name) == Chunk("id", "name"))
        p.fields.map(_.fieldType) match
          case Chunk(idType: Structure.Type.Primitive, nameType: Structure.Type.Primitive) =>
            assert(idType.kind == Structure.PrimitiveKind.Int)
            assert(nameType.kind == Structure.PrimitiveKind.String)
          case other =>
            assert(false, s"expected two Primitive field types, got $other")
      case other =>
        assert(false, s"expected a Product, got $other")
  }

  "a Record decodes into any type with a matching field shape, not just the type that produced it" in {
    // If Structure.Value carried nominal identity (e.g. a class name or tag on Record itself),
    // decoding Customer's encoding as the unrelated Widget would be expected to fail. It succeeds,
    // which is the direct evidence that the Value tier is purely structural.
    val dynamic = Structure.encode(ada)
    assert(Structure.decode[Widget](dynamic) == Result.succeed(Widget(1, "Ada")))
  }

  "decode still fails when the field shape genuinely differs (by name), so this isn't decode-anything" in {
    val dynamic = Structure.encode(ada)
    assert(Structure.decode[Wrong](dynamic).isFailure)
  }

  "the Type tier, unlike the Value tier, does carry the Scala type's name" in {
    // Contrast case for the finding above: Structure.Type.Product.name is populated from the
    // Scala class (here "Customer" vs "Widget") even though the two types are structurally
    // compatible. That name is not a Morphir FQName and is never reached once encode() has
    // produced a Structure.Value - it exists only on the compile-time Type side.
    val customerType = Structure.of[Customer].asInstanceOf[Structure.Type.Product]
    val widgetType   = Structure.of[Widget].asInstanceOf[Structure.Type.Product]
    assert(customerType.name == "Customer")
    assert(widgetType.name == "Widget")
    assert(customerType.name != widgetType.name)
    assert(Structure.Type.compatible(customerType, widgetType))
  }

end StructureNominalitySpec
