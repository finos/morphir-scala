package org.finos.morphir.codemodel.compat.v3

import kyo.*
import kyo.Json.given_Json
import kyo.test.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.*

/** What the projection writes for the 3.1.0 additions: the release, `Specs`, module documentation, derived types. */
class V3WireProjectionReleaseTests extends Test[Any]:

  private def str(value: String): Structure.Value = Structure.Value.Str(value)

  private def encoded(projection: Result[V3ProjectionError, Structure.Value]): Maybe[String] =
    projection.map(Json.encode(_)).toMaybe

  private val packageName = PackageName.fromString("local/example")
  private val moduleName  = ModuleName.fromString("Example")

  private def specsWith(doc: Option[cm.Documentation]) = cm.Distribution.Specs(
    cm.SpecsDistribution(
      cm.PackageInfo(packageName, ""),
      cm.PackageSpecification(Map(moduleName -> cm.ModuleSpecification(Map.empty, Map.empty, doc))),
      Map.empty
    )
  )

  private val library = cm.Distribution.Library(
    cm.LibraryDistribution(
      cm.PackageInfo(packageName, ""),
      cm.PackageDefinition(
        Map(moduleName -> cm.AccessControlled(cm.Access.Public, cm.ModuleDefinition(Map.empty, Map.empty)))
      ),
      Map.empty
    )
  )

  "a Specs distribution" - {
    "is written at 3.1.0, the lowest release that has it, with its module documentation" in
      assert(
        encoded(V3WireProjection.project(specsWith(Some(cm.Documentation(Chunk("Basics."))))))
          == Present(
            """{"formatVersion":"3.1.0","distribution":["Specs",[["local"],["example"]],[],""" +
              """{"modules":[[[["example"]],{"types":[],"values":[],"doc":"Basics."}]]}]}"""
          )
      )
    "is refused at a release below 3.1.0" in
      assert(
        V3WireProjection.project(specsWith(None), V3AttributeMode.InferredTypes, V3Release.Baseline) ==
          Result.fail(V3ProjectionError.UnsupportedDistribution("Specs at format version 3.0.0, below 3.1.0"))
      )
  }

  "a module" - {
    "always has a doc member, null without documentation, as morphir-elm writes it" in
      assert(
        encoded(V3WireProjection.project(library)) == Present(
          """{"formatVersion":3,"distribution":["Library",[["local"],["example"]],[],""" +
            """{"modules":[[[["example"]],{"access":"Public","value":{"types":[],"values":[],"doc":null}}]]}]}"""
        )
      )
    "joins its documentation lines with a line break" in {
      val spec = cm.ModuleSpecification(Map.empty, Map.empty, Some(cm.Documentation(Chunk("one", "two"))))
      assert(
        encoded(V3WireProjection.Nodes.moduleSpecification(spec)) ==
          Present("""{"types":[],"values":[],"doc":"one\ntwo"}""")
      )
    }
  }

  "the release" - {
    "is written as read: the integer 3 for 3.0.0, the release string otherwise" in {
      val later = V3Release.of(1, 2).getOrElse(V3Release.Baseline)
      assert(
        encoded(V3WireProjection.project(library, V3AttributeMode.InferredTypes, later))
          .exists(_.startsWith("""{"formatVersion":"3.1.2",""""))
      )
      assert(V3WireProjection.formatVersion(V3Release.Baseline) == Structure.Value.Integer(3))
      assert(V3WireProjection.formatVersion(V3Release.Specs) == str("3.1.0"))
    }
  }

  "a derived type specification (versions-0007)" - {
    "is written as its tag, parameters and details, and reads back" in {
      val json =
        """["DerivedTypeSpecification",[],{"baseType":["Reference",{},[[["morphir"],["s","d","k"]],[["string"]],""" +
          """["string"]],[]],"fromBaseType":[[["my"],["org"]],[["module"]],["from","string"]],""" +
          """"toBaseType":[[["my"],["org"]],[["module"]],["to","string"]]}]"""
      val decoded = V3Reader.decodeTypeSpecification(json)
      assert(decoded.toMaybe.exists(_.isInstanceOf[cm.TypeSpecification.DerivedTypeSpecification]))
      assert(decoded.flatMap(V3WireProjection.Nodes.typeSpecification).map(Json.encode(_)).toMaybe == Present(json))
    }
    "requires all three details" in {
      val json = """["DerivedTypeSpecification",[],{"baseType":["Unit",{}],"fromBaseType":[[],[],["f"]]}]"""
      assert(
        V3Reader.decodeTypeSpecification(json) ==
          Result.fail(V3ReadError.WrongShape("$[2].toBaseType", "a toBaseType member"))
      )
    }
  }
end V3WireProjectionReleaseTests
