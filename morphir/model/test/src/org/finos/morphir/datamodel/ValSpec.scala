package org.finos.morphir.datamodel

import kyo.test.*
import kyo.{Chunk, Result, Schema, Structure}
import kyo.Json.given_Json
import org.finos.morphir.naming.*
import org.finos.morphir.codemodel.{Expr, Literal, ValueAttributes}

/**
 * The closure round-trip below is the load-bearing assertion for this whole slice: it is the property the classic
 * runtime's `RTValue` can never have (it closes over a Scala function, which cannot be serialized), and it is what
 * makes snapshot, resume, and deterministic replay possible on top of `Val`.
 */
class ValSpec extends Test[Any]:

  private val closure: Val =
    Val.Closure(
      params = Chunk(Name.fromString("x")),
      body = Expr.Literal(ValueAttributes.empty, Literal.IntegerLiteral(BigInt(42))),
      env = Map(Name.fromString("y") -> Val.Structured(Structure.encode(7)))
    )

  "round-trips a closure through JSON" in {
    val schema = summon[Schema[Val]]
    assert(schema.decodeString(schema.encodeString(closure)) == Result.succeed(closure))
  }

  "round-trips structured data through JSON" in {
    val v: Val = Val.Structured(Structure.encode("hello"))
    val schema = summon[Schema[Val]]
    assert(schema.decodeString(schema.encodeString(v)) == Result.succeed(v))
  }

  "round-trips a partial application through JSON" in {
    val v: Val =
      Val.Partial(
        fqn = FQName.fromString("Morphir.SDK:Basics:add"),
        arity = 2,
        applied = Chunk(Val.Structured(Structure.encode(1)))
      )
    val schema = summon[Schema[Val]]
    assert(schema.decodeString(schema.encodeString(v)) == Result.succeed(v))
  }

end ValSpec
