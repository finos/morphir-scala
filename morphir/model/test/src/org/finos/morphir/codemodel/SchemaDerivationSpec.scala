package org.finos.morphir.codemodel

import kyo.test.*
import kyo.{Chunk, Schema}
import kyo.Json.given_Json
import org.finos.morphir.naming.*

/**
 * Spike R1/R2: does kyo-schema's `Schema` derive over the code model's mutually recursive `Type`/`Expr` closure, and
 * does it round-trip through JSON.
 */
class SchemaDerivationSpec extends Test[Any]:

  // A nested type: Reference wrapping a Record wrapping a Variable.
  private val nested: Type =
    Type.Reference(
      TypeAttributes.empty,
      FQName.fromString("Morphir.SDK:Basics:maybe"),
      Chunk(
        Type.Record(
          TypeAttributes.empty,
          Chunk(Field(Name.fromString("value"), Type.Variable(TypeAttributes.empty, Name.fromString("a"))))
        )
      )
    )

  private val expr: Expr =
    Expr.Apply(
      ValueAttributes.empty,
      Expr.Reference(ValueAttributes.empty, FQName.fromString("Morphir.SDK:Basics:add")),
      Expr.Literal(ValueAttributes.empty, Literal.IntegerLiteral(BigInt(1)))
    )

  "derives a Schema for the recursive Type tree" in {
    val schema = summon[Schema[Type]]
    assert(schema != null)
  }

  "round-trips a nested Type through JSON" in {
    val schema  = summon[Schema[Type]]
    val encoded = schema.encodeString(nested)
    val decoded = schema.decodeString(encoded)
    assert(decoded == nested)
  }

  "derives a Schema for the recursive Expr tree" in {
    val schema = summon[Schema[Expr]]
    assert(schema != null)
  }

  "round-trips a nested Expr through JSON" in {
    val schema  = summon[Schema[Expr]]
    val encoded = schema.encodeString(expr)
    val decoded = schema.decodeString(encoded)
    assert(decoded == expr)
  }

end SchemaDerivationSpec
