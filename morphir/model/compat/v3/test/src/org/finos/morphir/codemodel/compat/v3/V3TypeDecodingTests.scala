package org.finos.morphir.codemodel.compat.v3

import kyo.*
import kyo.test.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.FQName

import V3ReadFixtures.*

/** Types, type definitions, type specifications and constructors; cases ported from the classic v3 decoder tests. */
class V3TypeDecodingTests extends Test[Any]:

  private val f = variable("f")
  private val g = variable("g")

  private val fieldsJson =
    """[{"name":["first"],"tpe":["Variable",123,["f"]]},{"name":["second"],"tpe":["Variable",345,["g"]]}]"""
  private val fields = Chunk(cm.Field(n("first"), f), cm.Field(n("second"), g))

  "Type" - {
    "decodes Unit" in
      assert(success(V3Reader.decodeType("""["Unit",1234]""")) == Present(cm.Type.Unit(noType)))
    "decodes Variable" in
      assert(success(V3Reader.decodeType("""["Variable",1234,["x"]]""")) == Present(variable("x")))
    "decodes Record" in
      assert(success(V3Reader.decodeType(s"""["Record",1,$fieldsJson]""")) == Present(cm.Type.Record(noType, fields)))
    "decodes ExtensibleRecord" in
      assert(
        success(V3Reader.decodeType(s"""["ExtensibleRecord",1,["some","name"],$fieldsJson]""")) ==
          Present(cm.Type.ExtensibleRecord(noType, n("some", "name"), fields))
      )
    "decodes Tuple" in
      assert(
        success(V3Reader.decodeType("""["Tuple",1,[["Variable",123,["f"]],["Variable",345,["g"]]]]""")) ==
          Present(cm.Type.Tuple(noType, Chunk(f, g)))
      )
    "decodes Reference" in {
      val json =
        """["Reference",1,[[["test"]],[["java","home"]],["morphir"]],[["Variable",123,["f"]],["Variable",345,["g"]]]]"""
      assert(success(V3Reader.decodeType(json)) == Present(cm.Type.Reference(noType, javaHomeMorphir, Chunk(f, g))))
    }
    "decodes Function" in
      assert(
        success(V3Reader.decodeType("""["Function",1,["Variable",123,["f"]],["Variable",345,["g"]]]""")) ==
          Present(cm.Type.Function(noType, f, g))
      )
    "erases a type attribute, whatever its shape" in {
      assert(success(V3Reader.decodeType("""["Unit",{"source":[1,2]}]""")) == Present(cm.Type.Unit(noType)))
      assert(success(V3Reader.decodeType("""["Unit",["Variable",{},["a"]]]""")) == Present(cm.Type.Unit(noType)))
    }
    "rejects an unknown type tag" in
      assert(failure(V3Reader.decodeType("""["Alias",{}]""")) == Present(V3ReadError.UnknownTag("$", "Alias")))
    "locates an unknown tag nested in a function type" in
      assert(
        failure(V3Reader.decodeType("""["Function",{},["Variable",{},["a"]],["Variabl",{},["b"]]]""")) ==
          Present(V3ReadError.UnknownTag("$[3]", "Variabl"))
      )
    "rejects a node with the wrong number of arguments" in
      assert(
        failure(V3Reader.decodeType("""["Variable",{}]""")) ==
          Present(V3ReadError.WrongShape("$", "Variable with 2 arguments after the tag"))
      )
    "rejects a record field without its type" in
      assert(
        failure(V3Reader.decodeType("""["Record",{},[{"name":["a"]}]]""")) ==
          Present(V3ReadError.WrongShape("$[2][0].tpe", "a tpe member"))
      )
  }

  private val constructorsJson =
    """[[["name","1"],[[["name","1"],["Variable",123,["f"]]],[["name","2"],["Variable",345,["g"]]]]],[["name","2"],[[["name","3"],["Variable",678,["h"]]],[["name","4"],["Variable",789,["i"]]]]]]"""

  private val constructors = Chunk(
    cm.Constructor(n("name", "1"), Chunk(cm.Parameter(n("name", "1"), f), cm.Parameter(n("name", "2"), g))),
    cm.Constructor(
      n("name", "2"),
      Chunk(cm.Parameter(n("name", "3"), variable("h")), cm.Parameter(n("name", "4"), variable("i")))
    )
  )
  private val params = Chunk(n("name", "1"), n("name", "2"))

  "TypeDefinition" - {
    "decodes TypeAliasDefinition" in
      assert(
        success(V3Reader.decodeTypeDefinition(
          """["TypeAliasDefinition",[["name","1"],["name","2"]],["Variable",345,["g"]]]"""
        )) ==
          Present(cm.TypeDefinition.TypeAliasDefinition(params, g))
      )
    "decodes CustomTypeDefinition" in {
      val json =
        s"""["CustomTypeDefinition",[["name","1"],["name","2"]],{"access":"Public","value":$constructorsJson}]"""
      assert(
        success(V3Reader.decodeTypeDefinition(json)) ==
          Present(cm.TypeDefinition.CustomTypeDefinition(params, cm.AccessControlled(cm.Access.Public, constructors)))
      )
    }
    "decodes private constructors" in {
      val json = """["CustomTypeDefinition",[],{"access":"Private","value":[]}]"""
      assert(
        success(V3Reader.decodeTypeDefinition(json)) ==
          Present(cm.TypeDefinition.CustomTypeDefinition(
            Chunk.empty,
            cm.AccessControlled(cm.Access.Private, Chunk.empty)
          ))
      )
    }
    "rejects an unknown access level" in {
      val json = """["CustomTypeDefinition",[],{"access":"Internal","value":[]}]"""
      assert(failure(V3Reader.decodeTypeDefinition(json)).map(_.location) == Present("$[2].access"))
    }
  }

  "TypeSpecification" - {
    "decodes TypeAliasSpecification" in
      assert(
        success(
          V3Reader.decodeTypeSpecification(
            """["TypeAliasSpecification",[["name","1"],["name","2"]],["Variable",345,["g"]]]"""
          )
        ) == Present(cm.TypeSpecification.TypeAliasSpecification(params, g))
      )
    "decodes CustomTypeSpecification" in {
      val json = s"""["CustomTypeSpecification",[["name","1"],["name","2"]],$constructorsJson]"""
      assert(
        success(V3Reader.decodeTypeSpecification(json)) ==
          Present(cm.TypeSpecification.CustomTypeSpecification(params, constructors))
      )
    }
    "decodes OpaqueTypeSpecification" in
      assert(
        success(V3Reader.decodeTypeSpecification("""["OpaqueTypeSpecification",[["name","1"],["name","2"]]]""")) ==
          Present(cm.TypeSpecification.OpaqueTypeSpecification(params))
      )
  }

  "constructors" - {
    "decode an empty constructor list" in
      assert(
        success(V3Reader.decodeTypeSpecification("""["CustomTypeSpecification",[],[]]""")) ==
          Present(cm.TypeSpecification.CustomTypeSpecification(Chunk.empty, Chunk.empty))
      )
    "do not depend on input order" in {
      // The ir-v3-ingestion spec's constructor-order scenario: the same constructors, listed in two orders.
      val red                      = """[["red"],[]]"""
      val green                    = """[["green"],[]]"""
      val blue                     = """[["blue"],[[["shade"],["Variable",{},["a"]]]]]"""
      def decode(ordered: String*) =
        success(V3Reader.decodeTypeSpecification(s"""["CustomTypeSpecification",[],[${ordered.mkString(",")}]]"""))
      val expected = cm.TypeSpecification.CustomTypeSpecification(
        Chunk.empty,
        Chunk(
          cm.Constructor(n("blue"), Chunk(cm.Parameter(n("shade"), variable("a")))),
          cm.Constructor(n("green"), Chunk.empty),
          cm.Constructor(n("red"), Chunk.empty)
        )
      )
      assert(decode(red, green, blue) == Present(expected))
      assert(decode(blue, red, green) == Present(expected))
    }
    "order by name word by word, not by rendered text" in {
      // ["my", "type"] and ["myType"] both render as "myType"; word by word, ["my", …] sorts first.
      val json  = """["CustomTypeSpecification",[],[[["my","type"],[]],[["my"],[]],[["myType"],[]]]]"""
      val names = success(V3Reader.decodeTypeSpecification(json)).map {
        case cm.TypeSpecification.CustomTypeSpecification(_, ctors) => ctors.map(_.name.toList).toList
        case _                                                      => Nil
      }
      assert(names == Present(List(List("my"), List("my", "type"), List("myType"))))
    }
    "keep the last of a repeated constructor name, as a v3 map does" in {
      val json = """["CustomTypeSpecification",[],[[["a"],[]],[["a"],[[["x"],["Unit",{}]]]]]]"""
      assert(
        success(V3Reader.decodeTypeSpecification(json)) ==
          Present(
            cm.TypeSpecification.CustomTypeSpecification(
              Chunk.empty,
              Chunk(cm.Constructor(n("a"), Chunk(cm.Parameter(n("x"), cm.Type.Unit(noType)))))
            )
          )
      )
    }
  }
end V3TypeDecodingTests
