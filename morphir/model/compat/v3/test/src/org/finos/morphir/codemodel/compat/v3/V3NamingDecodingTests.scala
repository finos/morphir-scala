package org.finos.morphir.codemodel.compat.v3

import kyo.*
import kyo.test.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.FQName
import org.finos.morphir.naming.ModuleName
import org.finos.morphir.naming.Name
import org.finos.morphir.naming.PackageName

import V3ReadFixtures.*

/** Names, paths, fully qualified names, literals and documentation; cases ported from the classic v3 decoder tests. */
class V3NamingDecodingTests extends Test[Any]:

  "Name" - {
    "decodes an empty name" in
      assert(success(V3Reader.decodeName("[]")) == Present(Name.fromList(Nil)))
    "decodes a single-word name" in
      assert(success(V3Reader.decodeName("""["hello"]""")) == Present(n("hello")))
    "decodes a multi-word name" in {
      assert(success(V3Reader.decodeName("""["hello","there"]""")) == Present(Name.fromString("HelloThere")))
      assert(success(V3Reader.decodeName("""["this","is","a","list"]""")) == Present(n("this", "is", "a", "list")))
    }
    "rejects a name that is not an array" in
      assert(failure(V3Reader.decodeName("\"hello\"")) == Present(V3ReadError.WrongShape("$", "an array")))
    "rejects a word that is not a string, at its index" in
      assert(failure(V3Reader.decodeName("""["a",1]""")) == Present(V3ReadError.WrongShape("$[1]", "a string")))
  }

  "Path" - {
    "decodes an empty path" in
      assert(success(V3Reader.decodePath("[]")) == Present(path()))
    "decodes a single-segment path" in
      assert(success(V3Reader.decodePath("""[["org"]]""")) == Present(path(n("org"))))
    "decodes a path" in
      assert(
        success(V3Reader.decodePath("""[["org"],["foo"],["bar"]]""")) == Present(path(n("org"), n("foo"), n("bar")))
      )
    "rejects a segment that is not a name, at its index" in
      assert(failure(V3Reader.decodePath("""[["org"],"foo"]""")) == Present(V3ReadError.WrongShape("$[1]", "an array")))
  }

  "FQName" - {
    "decodes an empty fully qualified name" in
      assert(
        success(V3Reader.decodeFQName("[[],[],[]]")) ==
          Present(FQName(PackageName(path()), ModuleName(path()), Name.fromList(Nil)))
      )
    "decodes a fully qualified name" in {
      val expected = FQName(pkg(n("com"), n("example")), mod(n("java", "home")), n("morphir"))
      assert(success(V3Reader.decodeFQName("""[[["com"],["example"]],[["java","home"]],["morphir"]]""")) ==
        Present(expected))
    }
    "rejects a fully qualified name without three parts" in
      assert(
        failure(V3Reader.decodeFQName("""[[["com"]],[["java"]]]""")) ==
          Present(V3ReadError.WrongShape("$", "an array of 3 elements"))
      )
    "locates a fault inside the module path" in
      assert(
        failure(V3Reader.decodeFQName("""[[["com"]],[["java",2]],["x"]]""")) ==
          Present(V3ReadError.WrongShape("$[1][0][1]", "a string"))
      )
  }

  "Literal" - {
    "decodes a BoolLiteral" in
      assert(success(V3Reader.decodeLiteral("""["BoolLiteral",true]""")) == Present(cm.Literal.BoolLiteral(true)))
    "decodes a CharLiteral" in
      assert(success(V3Reader.decodeLiteral("""["CharLiteral","x"]""")) == Present(cm.Literal.CharLiteral("x")))
    "decodes a CharLiteral outside the basic multilingual plane" in
      assert(
        success(V3Reader.decodeLiteral("[\"CharLiteral\",\"😀\"]")) ==
          Present(cm.Literal.CharLiteral("😀"))
      )
    "decodes a DecimalLiteral" in
      assert(
        success(V3Reader.decodeLiteral("""["DecimalLiteral","1.23456789"]""")) ==
          Present(cm.Literal.DecimalLiteral(BigDecimal("1.23456789")))
      )
    "decodes a FloatLiteral" in
      assert(success(V3Reader.decodeLiteral("""["FloatLiteral",1.3232]""")) ==
        Present(cm.Literal.FloatLiteral(1.3232d)))
    "decodes a FloatLiteral written as a whole number" in
      assert(success(V3Reader.decodeLiteral("""["FloatLiteral",2]""")) == Present(cm.Literal.FloatLiteral(2.0d)))
    "decodes a StringLiteral" in
      assert(
        success(V3Reader.decodeLiteral("""["StringLiteral","hello"]""")) == Present(cm.Literal.StringLiteral("hello"))
      )
    "decodes a WholeNumberLiteral" in
      assert(
        success(V3Reader.decodeLiteral("""["WholeNumberLiteral",321321]""")) ==
          Present(cm.Literal.IntegerLiteral(BigInt(321321)))
      )
    "rejects an unknown literal tag" in
      assert(
        failure(V3Reader.decodeLiteral("""["ByteLiteral",1]""")) == Present(V3ReadError.UnknownTag("$", "ByteLiteral"))
      )
    "rejects a CharLiteral of more than one character" in
      assert(failure(V3Reader.decodeLiteral("""["CharLiteral","xy"]""")).map(_.location) == Present("$[1]"))
    "rejects a WholeNumberLiteral with a fraction" in
      assert(failure(V3Reader.decodeLiteral("""["WholeNumberLiteral",1.5]""")).map(_.location) == Present("$[1]"))
    "rejects a DecimalLiteral that is not a number" in
      assert(failure(V3Reader.decodeLiteral("""["DecimalLiteral","ten"]""")).map(_.location) == Present("$[1]"))
    "rejects a literal without its argument" in
      assert(
        failure(V3Reader.decodeLiteral("""["BoolLiteral"]""")) ==
          Present(V3ReadError.WrongShape("$", "BoolLiteral with 1 argument after the tag"))
      )
  }

  "documentation" - {
    def documentationOf(doc: String): Maybe[Option[cm.Documentation]] =
      val json =
        s"""{"types":[[["t"],{"doc":$doc,"value":["OpaqueTypeSpecification",[]]}]],"values":[]}"""
      success(
        V3Reader.decodeModuleSpecification(json)
      ).flatMap(spec => Maybe.fromOption(spec.types.get(n("t")))).map(_.doc)

    "decodes an empty doc string as no documentation" in
      assert(documentationOf("\"\"") == Present(None))
    "decodes a doc string as its lines" in {
      assert(documentationOf("\"typeDoc1\"") == Present(Some(cm.Documentation(Chunk("typeDoc1")))))
      assert(documentationOf("\"first\\nsecond\"") == Present(Some(cm.Documentation(Chunk("first", "second")))))
    }
    "requires the doc member" in {
      val json = """{"types":[[["t"],{"value":["OpaqueTypeSpecification",[]]}]],"values":[]}"""
      assert(
        failure(V3Reader.decodeModuleSpecification(json)) ==
          Present(V3ReadError.WrongShape("$.types[0][1].doc", "a doc member"))
      )
    }
  }
end V3NamingDecodingTests
