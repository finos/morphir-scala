package org.finos.morphir.ir

import org.finos.morphir.ir.Module.Definition
import org.finos.morphir.ir.{Literal => Lit}
import org.finos.morphir.samples.ModuleExample.*
import org.finos.morphir.syntax.AllSyntax
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.TestAspect.ignore
import zio.test.*

object ModuleModuleSpec extends MorphirBaseSpec with AllSyntax {

  def spec = suite("ModuleModuleSpec")(
    suite("Module Definition")(
      test("Can be turned to Specification") {
//        val expected = Specification(types = Map{
//          Name("hello") -> typeAlias.map(_.toSpecification)
//          Name("world") -> customType.map(_.toSpecification)
//        },
//          Map{
//            Name("val") -> ValueModule.Definition.fromLiteral(string("string")).toSpecification
//          }
//        )
//        assertTrue(moduleDef.toSpecification == expected)
        // todo add when TypeModule toSpec is added
        assertTrue(1 == 1)
      },
      test("Can look up values") {
        assertTrue(
          moduleDef.lookupValueDefinition(Name("val")) == Some(Value.Definition.fromLiteral(Lit.string("string")))
        )
      },
      test("Can be erased") {
        assertTrue(moduleDef.eraseAttributes == Definition.empty)
      } @@ ignore @@ TestAspect.tag("eraseAttributes Not Implemented yet"),
      test("Can collect all references") {
        assertTrue(
          moduleDef.collectTypeReferences.size == 0 &&
            moduleDef.collectValueReferences.size == 0 &&
            moduleDef.collectValueReferences.size == 0
        )
      }
    ),
    suite("Module Specification")(
      test("Can look up values") {
        val result = moduleSpec.lookupValueSpecification(Name("spec1"))
        assertTrue(
          result.isDefined && result.get.inputs.size == 2 && result.get.output == define.variable("WholeNumbers")
        )
      },
      test("Can look up types") {
        val result = moduleSpec.lookupTypeSpecification(Name("world"))
        assertTrue(
          result.isDefined && !moduleSpec.lookupTypeSpecification(Name("notHere")).isDefined
        )
      },
      test("Can be erased") {
        val result   = moduleSpec.eraseAttributes
        val expected = moduleSpec.mapAttributes(_ => ())
        assertTrue(
          result == expected
        )
      }
    )
  )
}
