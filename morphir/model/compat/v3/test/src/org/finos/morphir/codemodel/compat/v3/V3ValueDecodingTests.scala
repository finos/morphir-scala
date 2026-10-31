package org.finos.morphir.codemodel.compat.v3

import kyo.*
import kyo.test.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.FQName

import V3ReadFixtures.*

/** Patterns, values, value definitions and value specifications; cases ported from the classic v3 decoder tests. */
class V3ValueDecodingTests extends Test[Any]:

  private val wildcard       = cm.Pattern.WildcardPattern(noValue)
  private val wildCardAlias  = cm.Pattern.AsPattern(noValue, wildcard, n("wild", "card"))
  private val unit           = cm.Expr.Unit(noValue)
  private val helloField     = cm.Expr.FieldFunction(noValue, n("hello"))
  private val comExampleJava = FQName(pkg(n("com"), n("example")), mod(n("java", "home")), n("morphir"))

  "Pattern" - {
    "decodes AsPattern" in
      assert(
        success(V3Reader.decodePattern("""["AsPattern",1,["WildcardPattern",1],["wild","card"]]""")) ==
          Present(wildCardAlias)
      )
    "decodes ConstructorPattern" in {
      val json =
        s"""["ConstructorPattern",1,$javaHomeMorphirJson,[["WildcardPattern",1],["EmptyListPattern",2],["AsPattern",1,["WildcardPattern",1],["wild","card"]]]]"""
      assert(
        success(V3Reader.decodePattern(json)) ==
          Present(
            cm.Pattern.ConstructorPattern(
              noValue,
              javaHomeMorphir,
              Chunk(wildcard, cm.Pattern.EmptyListPattern(noValue), wildCardAlias)
            )
          )
      )
    }
    "decodes EmptyListPattern" in
      assert(success(V3Reader.decodePattern("""["EmptyListPattern",1]""")) ==
        Present(cm.Pattern.EmptyListPattern(noValue)))
    "decodes LiteralPattern" in
      assert(
        success(V3Reader.decodePattern("""["LiteralPattern",1,["StringLiteral","hello"]]""")) ==
          Present(cm.Pattern.LiteralPattern(noValue, cm.Literal.StringLiteral("hello")))
      )
    "decodes HeadTailPattern" in
      assert(
        success(V3Reader.decodePattern("""["HeadTailPattern",1,["WildcardPattern",1],["EmptyListPattern",2]]""")) ==
          Present(cm.Pattern.HeadTailPattern(noValue, wildcard, cm.Pattern.EmptyListPattern(noValue)))
      )
    "decodes TuplePattern" in {
      val json =
        """["TuplePattern",1,[["WildcardPattern",1],["UnitPattern",2],["AsPattern",1,["WildcardPattern",1],["wild","card"]]]]"""
      assert(
        success(V3Reader.decodePattern(json)) ==
          Present(cm.Pattern.TuplePattern(noValue, Chunk(wildcard, cm.Pattern.UnitPattern(noValue), wildCardAlias)))
      )
    }
    "decodes UnitPattern" in
      assert(success(V3Reader.decodePattern("""["UnitPattern",1]""")) == Present(cm.Pattern.UnitPattern(noValue)))
    "decodes WildcardPattern" in
      assert(success(V3Reader.decodePattern("""["WildcardPattern",1]""")) == Present(wildcard))
    "keeps a typed pattern's type" in
      assert(
        success(V3Reader.decodePattern(s"""["WildcardPattern",$intTypeJson]""")) ==
          Present(cm.Pattern.WildcardPattern(typed(intType)))
      )
    "rejects an unknown pattern tag" in
      assert(failure(V3Reader.decodePattern("""["ListPattern",{}]""")) ==
        Present(V3ReadError.UnknownTag("$", "ListPattern")))
  }

  "Value" - {
    "decodes Apply" in
      assert(success(V3Reader.decodeValue("""["Apply",3,["Unit",6],["Unit",6]]""")) ==
        Present(cm.Expr.Apply(noValue, unit, unit)))
    "decodes Constructor" in
      assert(
        success(V3Reader.decodeValue("""["Constructor",3,[[["com"],["example"]],[["java","home"]],["morphir"]]]""")) ==
          Present(cm.Expr.Constructor(noValue, comExampleJava))
      )
    "decodes Destructure" in
      assert(
        success(V3Reader.decodeValue("""["Destructure",3,["WildcardPattern",1],["Unit",6],["Unit",6]]""")) ==
          Present(cm.Expr.Destructure(noValue, wildcard, unit, unit))
      )
    "decodes Field" in
      assert(
        success(V3Reader.decodeValue("""["Field",3,["Unit",6],["hello"]]""")) ==
          Present(cm.Expr.Field(noValue, unit, n("hello")))
      )
    "decodes FieldFunction" in
      assert(success(V3Reader.decodeValue("""["FieldFunction",3,["hello"]]""")) == Present(helloField))
    "decodes IfThenElse" in
      assert(
        success(V3Reader.decodeValue("""["IfThenElse",3,["Unit",6],["FieldFunction",3,["hello"]],["Unit",6]]""")) ==
          Present(cm.Expr.IfThenElse(noValue, unit, helloField, unit))
      )
    "decodes Lambda" in
      assert(
        success(V3Reader.decodeValue("""["Lambda",3,["WildcardPattern",1],["FieldFunction",3,["hello"]]]""")) ==
          Present(cm.Expr.Lambda(noValue, wildcard, helloField))
      )
    "decodes LetDefinition" in {
      val json =
        """["LetDefinition",3,["hi"],{"inputTypes":[[["name","1"],1,["Variable",444,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",345,["g"]],"body":["Literal",3,["BoolLiteral",true]]},["FieldFunction",3,["hello"]]]"""
      val definition = cm.ValueDefinitionBody.ExpressionBody(
        Chunk(cm.Parameter(n("name", "1"), variable("g")), cm.Parameter(n("name", "2"), variable("h"))),
        variable("g"),
        cm.Expr.Literal(noValue, cm.Literal.BoolLiteral(true))
      )
      assert(
        success(V3Reader.decodeValue(json)) == Present(cm.Expr.LetDefinition(noValue, n("hi"), definition, helloField))
      )
    }
    "decodes LetRecursion, ordering its definitions by name" in {
      def definition(output: String) =
        s"""{"inputTypes":[],"outputType":["Variable",333,["$output"]],"body":["Unit",3]}"""
      val json =
        s"""["LetRecursion",3,[[["key","2"],${definition("y")}],[["key","1"],${definition(
            "x"
          )}]],["FieldFunction",3,["hello"]]]"""
      def body(output: String) = cm.ValueDefinitionBody.ExpressionBody(Chunk.empty, variable(output), unit)
      assert(
        success(V3Reader.decodeValue(json)) ==
          Present(
            cm.Expr.LetRecursion(
              noValue,
              Chunk(cm.Binding(n("key", "1"), body("x")), cm.Binding(n("key", "2"), body("y"))),
              helloField
            )
          )
      )
    }
    "decodes List" in
      assert(
        success(V3Reader.decodeValue("""["List",3,[["Unit",6],["FieldFunction",3,["hello"]]]]""")) ==
          Present(cm.Expr.List(noValue, Chunk(unit, helloField)))
      )
    "decodes Literal" in
      assert(
        success(V3Reader.decodeValue("""["Literal",3,["BoolLiteral",true]]""")) ==
          Present(cm.Expr.Literal(noValue, cm.Literal.BoolLiteral(true)))
      )
    "decodes PatternMatch" in
      assert(
        success(V3Reader.decodeValue(
          """["PatternMatch",3,["Unit",6],[[["WildcardPattern",12],["FieldFunction",3,["hello"]]]]]"""
        )) ==
          Present(cm.Expr.PatternMatch(noValue, unit, Chunk(cm.MatchCase(wildcard, helloField))))
      )
    "decodes Record, keeping its field order" in
      assert(
        success(
          V3Reader.decodeValue("""["Record",3,[[["there"],["Unit",6]],[["hello"],["FieldFunction",3,["hello"]]]]]""")
        ) ==
          Present(
            cm.Expr.Record(noValue, Chunk(cm.RecordField(n("there"), unit), cm.RecordField(n("hello"), helloField)))
          )
      )
    "decodes Reference" in
      assert(
        success(V3Reader.decodeValue("""["Reference",3,[[["com"],["example"]],[["java","home"]],["morphir"]]]""")) ==
          Present(cm.Expr.Reference(noValue, comExampleJava))
      )
    "decodes Tuple" in
      assert(
        success(V3Reader.decodeValue("""["Tuple",3,[["Unit",6],["FieldFunction",3,["hello"]]]]""")) ==
          Present(cm.Expr.Tuple(noValue, Chunk(unit, helloField)))
      )
    "decodes UpdateRecord, ordering its updates by name" in
      assert(
        success(V3Reader.decodeValue(
          """["UpdateRecord",3,["Unit",6],[[["there"],["Unit",6]],[["hello"],["FieldFunction",3,["hello"]]]]]"""
        )) ==
          Present(
            cm.Expr.UpdateRecord(
              noValue,
              unit,
              Chunk(cm.RecordField(n("hello"), helloField), cm.RecordField(n("there"), unit))
            )
          )
      )
    "decodes Unit" in
      assert(success(V3Reader.decodeValue("""["Unit",6]""")) == Present(unit))
    "decodes Variable" in
      assert(success(V3Reader.decodeValue("""["Variable",3,["hello"]]""")) ==
        Present(cm.Expr.Variable(noValue, n("hello"))))
    "rejects an unknown value tag" in
      assert(failure(V3Reader.decodeValue("""["Hole",{}]""")) == Present(V3ReadError.UnknownTag("$", "Hole")))
    "locates a fault deep in a value" in {
      val json = """["Apply",{},["Lambda",{},["WildcardPattern",{}],["Variable",{},[7]]],["Unit",{}]]"""
      assert(failure(V3Reader.decodeValue(json)) == Present(V3ReadError.WrongShape("$[2][3][2][0]", "a string")))
    }
  }

  "attributes" - {
    // The ir-v3-ingestion spec's two attribute scenarios.
    "a typed value keeps its type as its inferred type" in {
      val json =
        s"""["Apply",$intTypeJson,["Variable",$intTypeJson,["f"]],["Literal",$intTypeJson,["WholeNumberLiteral",1]]]"""
      assert(
        success(V3Reader.decodeValue(json)) ==
          Present(
            cm.Expr.Apply(
              typed(intType),
              cm.Expr.Variable(typed(intType), n("f")),
              cm.Expr.Literal(typed(intType), cm.Literal.IntegerLiteral(BigInt(1)))
            )
          )
      )
    }
    "a typed value's type has its own attributes erased" in
      assert(
        success(V3Reader.decodeValue("""["Unit",["Unit",{"anything":true}]]""")) ==
          Present(cm.Expr.Unit(typed(cm.Type.Unit(noType))))
      )
    "an untyped attribute is dropped" in {
      assert(success(V3Reader.decodeValue("""["Variable",{},["x"]]""")) == Present(cm.Expr.Variable(noValue, n("x"))))
      assert(success(V3Reader.decodeValue("""["Unit",null]""")) == Present(unit))
      assert(success(V3Reader.decodeValue("""["Unit",[]]""")) == Present(unit))
      assert(success(V3Reader.decodePattern("""["UnitPattern",{"source":"x"}]""")) ==
        Present(cm.Pattern.UnitPattern(noValue)))
    }
    "a malformed type attribute is a fault, not dropped" in
      assert(failure(V3Reader.decodeValue("""["Unit",["Nope",{}]]""")) ==
        Present(V3ReadError.UnknownTag("$[1]", "Nope")))
  }

  "ValueDefinition" - {
    "decodes a value definition, erasing input attributes" in {
      val json =
        """{"inputTypes":[[["name","1"],1,["Variable",345,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",345,["g"]],"body":["Unit",1]}"""
      assert(
        success(V3Reader.decodeValueDefinition(json)) ==
          Present(
            cm.ValueDefinition(
              cm.AccessControlled(
                cm.Access.Public,
                cm.ValueDefinitionBody.ExpressionBody(
                  Chunk(cm.Parameter(n("name", "1"), variable("g")), cm.Parameter(n("name", "2"), variable("h"))),
                  variable("g"),
                  unit
                )
              )
            )
          )
      )
    }
    "requires a body" in
      assert(
        failure(V3Reader.decodeValueDefinition("""{"inputTypes":[],"outputType":["Unit",{}]}""")) ==
          Present(V3ReadError.WrongShape("$.body", "a body member"))
      )
    "rejects an input that is not a triple" in
      assert(
        failure(V3Reader.decodeValueDefinition(
          """{"inputTypes":[[["a"],["Unit",{}]]],"outputType":["Unit",{}],"body":["Unit",{}]}"""
        )) ==
          Present(V3ReadError.WrongShape("$.inputTypes[0]", "an array of 3 elements"))
      )
  }

  "ValueSpecification" - {
    "decodes a value specification" in {
      val json =
        """{"inputs":[[["name","1"],["Variable",345,["g"]]],[["name","2"],["Variable",678,["h"]]]],"output":["Variable",111,["f"]]}"""
      assert(
        success(V3Reader.decodeValueSpecification(json)) ==
          Present(
            cm.ValueSpecification(
              Chunk(cm.Parameter(n("name", "1"), variable("g")), cm.Parameter(n("name", "2"), variable("h"))),
              variable("f")
            )
          )
      )
    }
  }
end V3ValueDecodingTests
