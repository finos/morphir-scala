package org.finos.morphir.codemodel.compat.v3

import kyo.*
import kyo.test.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.Name
import org.finos.morphir.naming.PackageName

import V3ReadFixtures.*

/** Modules, packages and distributions; the library fixture is ported from the classic v3 decoder tests. */
class V3DistributionDecodingTests extends Test[Any]:

  private val unitJson   = """["Unit",{}]"""
  private val unitType   = cm.Type.Unit(noType)
  private val entryName  = n("name")
  private val params     = Chunk(n("name", "1"), n("name", "2"))
  private val orgSrc     = mod(n("org"), n("src"))
  private val orgTest    = mod(n("org"), n("test"))
  private val dependency = pkg(n("org"), n("finos"), n("morphir"), n("ir"))

  private val moduleSpecJson =
    s"""{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],$unitJson]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],$unitJson],[["name","2"],$unitJson]],"output":$unitJson}}]]}"""

  private val moduleSpec = cm.ModuleSpecification(
    Map(entryName -> cm.Documented(doc("typeDoc1"), cm.TypeSpecification.TypeAliasSpecification(params, unitType))),
    Map(
      entryName -> cm.Documented(
        doc("valueDoc1"),
        cm.ValueSpecification(
          Chunk(cm.Parameter(n("name", "1"), unitType), cm.Parameter(n("name", "2"), unitType)),
          unitType
        )
      )
    )
  )

  private def moduleDefJson(valueAccess: String) =
    s"""{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],$unitJson]}}]],"values":[[["name"],{"access":"$valueAccess","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],$unitJson,$unitJson],[["name","2"],$unitJson,$unitJson]],"outputType":$unitJson,"body":["Constructor",$unitJson,$javaHomeMorphirJson]}}}]],"doc":null}"""

  private def moduleDef(valueAccess: cm.Access) = cm.ModuleDefinition(
    Map(
      entryName -> cm.AccessControlled(
        cm.Access.Private,
        cm.Documented(doc("typeDoc1"), cm.TypeDefinition.TypeAliasDefinition(params, unitType))
      )
    ),
    Map(
      entryName -> cm.AccessControlled(
        valueAccess,
        cm.Documented(
          doc("valueDoc1"),
          cm.ValueDefinition(
            cm.AccessControlled(
              valueAccess,
              cm.ValueDefinitionBody.ExpressionBody(
                Chunk(cm.Parameter(n("name", "1"), unitType), cm.Parameter(n("name", "2"), unitType)),
                unitType,
                cm.Expr.Constructor(typed(unitType), javaHomeMorphir)
              )
            )
          )
        )
      )
    )
  )

  private def doc(line: String) = Some(cm.Documentation(Chunk(line)))

  private val packageSpecJson =
    s"""{"modules":[[[["org"],["src"]],$moduleSpecJson],[[["org"],["test"]],$moduleSpecJson]]}"""
  private val packageDefJson =
    s"""{"modules":[[[["org"],["src"]],{"access":"Public","value":${moduleDefJson(
        "Private"
      )}}],[[["org"],["test"]],{"access":"Private","value":${moduleDefJson("Private")}}]]}"""
  private val dependenciesJson = s"""[[[["org"],["finos"],["morphir"],["ir"]],$packageSpecJson]]"""

  private def libraryOf(packageName: PackageName): cm.Distribution =
    cm.Distribution.Library(
      cm.LibraryDistribution(
        cm.PackageInfo(packageName, ""),
        cm.PackageDefinition(
          Map(
            orgSrc  -> cm.AccessControlled(cm.Access.Public, moduleDef(cm.Access.Private)),
            orgTest -> cm.AccessControlled(cm.Access.Private, moduleDef(cm.Access.Private))
          )
        ),
        Map(dependency -> cm.PackageSpecification(Map(orgSrc -> moduleSpec, orgTest -> moduleSpec)))
      )
    )

  private val sdkJson = """[["morphir"],["s","d","k"]]"""

  "ModuleSpecification" - {
    "decodes a module specification" in
      assert(success(V3Reader.decodeModuleSpecification(moduleSpecJson)) == Present(moduleSpec))
  }

  "ModuleDefinition" - {
    "decodes a module definition, ignoring the module doc member" in
      assert(success(V3Reader.decodeModuleDefinition(moduleDefJson("Private"))) ==
        Present(moduleDef(cm.Access.Private)))
    "gives a value's definition body the access of its module entry" in
      assert(success(V3Reader.decodeModuleDefinition(moduleDefJson("Public"))) == Present(moduleDef(cm.Access.Public)))
    "locates a fault in a value body" in {
      val json = moduleDefJson("Public").replace("\"Constructor\"", "\"Constructr\"")
      assert(
        failure(V3Reader.decodeModuleDefinition(json)) ==
          Present(V3ReadError.UnknownTag("$.values[0][1].value.value.body", "Constructr"))
      )
    }
  }

  "Library" - {
    "decodes a library distribution" in {
      val json = s"""["Library",$sdkJson,$dependenciesJson,$packageDefJson]"""
      assert(success(V3Reader.decodeDistribution(json)) == Present(Chunk(libraryOf(sdk))))
    }
    "decodes a library from a morphir-ir.json document" in {
      val json = s"""{"formatVersion":3,"distribution":["Library",$sdkJson,$dependenciesJson,$packageDefJson]}"""
      assert(success(V3Reader.decodeIRFile(json)) == Present(Chunk(libraryOf(sdk))))
    }
    "locates a fault from the document root" in {
      val json =
        s"""{"formatVersion":3,"distribution":["Library",$sdkJson,[],{"modules":[[[["m"]],{"access":"Public","value":{"types":[],"values":[[["v"],{"access":"Public","value":{"doc":"","value":{"inputTypes":[],"outputType":["Unit",{}],"body":["Unit",{},1]}}}]]}}]]}]}"""
      assert(
        failure(V3Reader.decodeIRFile(json)) ==
          Present(
            V3ReadError.WrongShape(
              "$.distribution[3].modules[0][1].value.values[0][1].value.value.body",
              "Unit with 1 argument after the tag"
            )
          )
      )
    }
  }

  "Bundle" - {
    // The ir-v3-ingestion spec's bundle scenario: two packages, listed out of order.
    "decodes one library per package, ordered by package name" in {
      val copy   = pkg(n("morphir"), n("s", "d", "k"), n("copy"))
      val lib    = s"""{"dependencies":$dependenciesJson,"packageDef":$packageDefJson}"""
      val json   = s"""["Bundle",[[[["morphir"],["s","d","k"],["copy"]],$lib],[$sdkJson,$lib]]]"""
      val result = success(V3Reader.decodeDistribution(json))
      assert(result == Present(Chunk(libraryOf(sdk), libraryOf(copy))))
    }
    "decodes an empty bundle to no libraries" in
      assert(success(V3Reader.decodeIRFile("""{"formatVersion":3,"distribution":["Bundle",[]]}""")) ==
        Present(Chunk.empty))
    "rejects a bundled library without its package definition" in {
      val json = s"""["Bundle",[[$sdkJson,{"dependencies":[]}]]]"""
      assert(failure(V3Reader.decodeDistribution(json)) ==
        Present(V3ReadError.WrongShape("$[1][0][1].packageDef", "a packageDef member")))
    }
  }

  "decode after projection" - {
    "reads back what V3WireProjection writes" in {
      // morphir-elm keeps one access per value; the code model's two markers agree, so the projection accepts it.
      val original = libraryOf(sdk)
      val encoded  = V3WireProjection.encode(original)
      val decoded  = encoded match
        case Result.Success(json) => success(V3Reader.decodeIRFile(json))
        case _                    => Absent
      assert(decoded == Present(Chunk(original)))
    }
    "reads back a documented module and a Specs distribution at the release it was written in" in {
      val specs = cm.Distribution.Specs(
        cm.SpecsDistribution(
          cm.PackageInfo(sdk, ""),
          cm.PackageSpecification(Map(orgSrc -> moduleSpec.copy(doc = Some(cm.Documentation(Chunk("Basics.")))))),
          Map.empty
        )
      )
      val decoded = V3WireProjection.encode(specs) match
        case Result.Success(json) => success(V3Reader.decodeIRDocument(json))
        case _                    => Absent
      assert(decoded == Present(V3Document(V3Release.Specs, Chunk(specs))))
    }
  }

  "module documentation" - {
    def docOf(doc: String): Maybe[Option[cm.Documentation]] =
      success(V3Reader.decodeModuleSpecification(s"""{"types":[],"values":[]$doc}""")).map(_.doc)

    "is none when the doc member is null or absent" in {
      assert(docOf(""","doc":null""") == Present(None))
      assert(docOf("") == Present(None))
    }
    "keeps a doc string line by line" in {
      assert(docOf(""","doc":"Basics."""") == Present(Some(cm.Documentation(Chunk("Basics.")))))
      assert(docOf(""","doc":"one\ntwo"""") == Present(Some(cm.Documentation(Chunk("one", "two")))))
    }
    "keeps an empty doc string apart from a missing one" in
      assert(docOf(""","doc":""""") == Present(Some(cm.Documentation(Chunk.empty))))
    "is read on a module definition too" in {
      val json = """{"types":[],"values":[],"doc":"Domain."}"""
      assert(success(V3Reader.decodeModuleDefinition(json)).map(_.doc) ==
        Present(Some(cm.Documentation(Chunk("Domain.")))))
    }
    "rejects a doc that is not a string" in
      assert(
        failure(V3Reader.decodeModuleSpecification("""{"types":[],"values":[],"doc":3}""")) ==
          Present(V3ReadError.WrongShape("$.doc", "a string"))
      )
  }

  "Specs" - {
    // distributions-0011: the classic counterpart of a v4 Specs distribution.
    val specsJson =
      """["Specs",[["my"],["pkg"]],[],{"modules":[[[["basics"]],{"types":[[["int"],{"doc":"","value":["OpaqueTypeSpecification",[]]}]],"values":[],"doc":"Basics."}]]}]"""
    val expected = cm.Distribution.Specs(
      cm.SpecsDistribution(
        cm.PackageInfo(pkg(n("my"), n("pkg")), ""),
        cm.PackageSpecification(
          Map(
            mod(n("basics")) -> cm.ModuleSpecification(
              Map(n("int") -> cm.Documented(None, cm.TypeSpecification.OpaqueTypeSpecification(Chunk.empty))),
              Map.empty,
              Some(cm.Documentation(Chunk("Basics.")))
            )
          )
        ),
        Map.empty
      )
    )

    "decodes a Specs distribution declared at 3.1.0" in
      assert(
        success(V3Reader.decodeIRDocument(s"""{"formatVersion":"3.1.0","distribution":$specsJson}""")) ==
          Present(V3Document(V3Release.Specs, Chunk(expected)))
      )
    "decodes a bare Specs distribution, which names no release" in
      assert(success(V3Reader.decodeDistribution(specsJson)) == Present(Chunk(expected)))
    "refuses a Specs distribution declared below 3.1.0" in {
      val error = failure(V3Reader.decodeIRFile(s"""{"formatVersion":3,"distribution":$specsJson}"""))
      assert(error == Present(V3ReadError.SpecsBefore31("3")))
      assert(error.map(_.code) == Present("specs_before_3_1"))
      assert(failure(V3Reader.decodeIRFile(s"""{"formatVersion":"3.0.1","distribution":$specsJson}""")).map(_.code) ==
        Present("specs_before_3_1"))
    }
    "refuses a module definition in a Specs distribution" in {
      val json =
        """["Specs",[["p"]],[],{"modules":[[[["m"]],{"access":"Public","value":{"types":[],"values":[]}}]]}]"""
      assert(failure(V3Reader.decodeDistribution(json)).map(_.code) == Present("invalid_type"))
    }
  }

  "typed libraries" - {
    // distributions-0013: morphir-elm's Library holds typed IR, so every value, pattern and input in it has a type.
    def library(value: String): String =
      s"""{"formatVersion":3,"distribution":["Library",[["p"]],[],{"modules":[[[["m"]],{"access":"Public","value":""" +
        s"""{"types":[],"values":[[["v"],{"access":"Public","value":{"doc":"","value":$value}}]],"doc":null}}]]}]}"""
    val valuePath = "$.distribution[3].modules[0][1].value.values[0][1].value.value"

    "refuses an untyped value" in {
      val error =
        failure(V3Reader.decodeIRFile(library("""{"inputTypes":[],"outputType":["Unit",{}],"body":["Unit",{}]}""")))
      assert(error.map(_.location) == Present(s"$valuePath.body[1]"))
      assert(error.map(_.code) == Present("invalid_type"))
    }
    "refuses an untyped pattern" in {
      val body  = """["Lambda",["Unit",{}],["WildcardPattern",{}],["Unit",["Unit",{}]]]"""
      val error =
        failure(V3Reader.decodeIRFile(library(s"""{"inputTypes":[],"outputType":["Unit",{}],"body":$body}""")))
      assert(error.map(_.location) == Present(s"$valuePath.body[2][1]"))
    }
    "refuses an untyped input" in {
      val definition =
        """{"inputTypes":[[["x"],{},["Unit",{}]]],"outputType":["Unit",{}],"body":["Unit",["Unit",{}]]}"""
      assert(failure(V3Reader.decodeIRFile(library(definition))).map(_.location) ==
        Present(s"$valuePath.inputTypes[0][1]"))
    }
    "accepts untyped values in a module definition read on its own" in {
      val json = """{"types":[],"values":[[["v"],{"access":"Public","value":{"doc":"","value":""" +
        """{"inputTypes":[[["x"],{},["Unit",{}]]],"outputType":["Unit",{}],"body":["Unit",{}]}}}]]}"""
      assert(success(V3Reader.decodeModuleDefinition(json)).isDefined)
    }
    "keeps an input's declared type when its attribute is the alias's expansion" in {
      // morphir-elm writes the expansion of an aliased input type as the input's attribute (5 inputs of
      // evaluator-tests do). The code model holds one type per input, so the declared alias is kept.
      val alias      = """["Reference",{},[[["my"]],[["m"]],["alias"]],[]]"""
      val definition =
        s"""{"inputTypes":[[["x"],["Unit",{}],$alias]],"outputType":["Unit",{}],"body":["Unit",["Unit",{}]]}"""
      val decoded = success(V3Reader.decodeIRFile(library(definition))).map { distributions =>
        distributions.collect { case cm.Distribution.Library(lib) =>
          lib.definition.modules.values.flatMap(_.value.values.values).map(_.value.value.body.value)
        }.flatten
      }
      val aliasType =
        cm.Type.Reference(noType, org.finos.morphir.naming.FQName(pkg(n("my")), mod(n("m")), n("alias")), Chunk.empty)
      assert(decoded.map(_.toList.collect { case b: cm.ValueDefinitionBody.ExpressionBody =>
        b.inputTypes.map(_.tpe)
      }) ==
        Present(List(Chunk(aliasType))))
    }
  }
end V3DistributionDecodingTests
