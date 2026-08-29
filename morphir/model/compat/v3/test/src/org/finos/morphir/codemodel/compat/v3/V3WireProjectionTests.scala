package org.finos.morphir.codemodel.compat.v3

import kyo.*
import kyo.Json.given_Json
import kyo.test.*
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.*

class V3WireProjectionTests extends Test[Any]:

  private val intType = cm.Type.Reference(
    cm.TypeAttributes.empty,
    FQName.fqn("Morphir.SDK", "Basics", "Int"),
    Chunk.empty
  )
  private val intAttributes = cm.ValueAttributes.empty.copy(inferredType = Some(intType))
  private val addName       = Name.fromString("add")
  private val moduleName    = ModuleName.fromString("Example")
  private val packageName   = PackageName.fromString("local/example")

  private val addBody = cm.Expr.Apply(
    intAttributes,
    cm.Expr.Apply(
      intAttributes,
      cm.Expr.Reference(intAttributes, FQName.fqn("Morphir.SDK", "Basics", "add")),
      cm.Expr.Variable(intAttributes, Name.fromString("left"))
    ),
    cm.Expr.Variable(intAttributes, Name.fromString("right"))
  )

  private val addDefinition = cm.ValueDefinition(
    cm.AccessControlled(
      cm.Access.Public,
      cm.ValueDefinitionBody.ExpressionBody(
        Chunk(
          cm.Parameter(Name.fromString("left"), intType),
          cm.Parameter(Name.fromString("right"), intType)
        ),
        intType,
        addBody
      )
    )
  )

  private def libraryWith(definition: cm.ValueDefinition = addDefinition): cm.Distribution =
    cm.Distribution.Library(
      cm.LibraryDistribution(
        cm.PackageInfo(packageName, ""),
        cm.PackageDefinition(
          Map(
            moduleName -> cm.AccessControlled(
              cm.Access.Public,
              cm.ModuleDefinition(
                Map.empty,
                Map(addName -> cm.AccessControlled(cm.Access.Public, cm.Documented(None, definition)))
              )
            )
          )
        ),
        Map.empty
      )
    )

  private def str(value: String): Structure.Value                         = Structure.Value.Str(value)
  private def integer(value: Int): Structure.Value                        = Structure.Value.Integer(value)
  private def sequence(values: Structure.Value*): Structure.Value         = Structure.Value.Sequence(Chunk.from(values))
  private def record(fields: (String, Structure.Value)*): Structure.Value =
    Structure.Value.Record(Chunk.from(fields))

  private def name(value: Name): Structure.Value                                = sequence(value.toList.map(str)*)
  private def renderPath(value: org.finos.morphir.naming.Path): Structure.Value =
    sequence(value.segments.map(name)*)
  private def fqName(value: FQName): Structure.Value =
    sequence(renderPath(value.packagePath.toPath), renderPath(value.modulePath.toPath), name(value.localName))

  private val expectedIntType = sequence(
    str("Reference"),
    record(),
    fqName(FQName.fqn("Morphir.SDK", "Basics", "Int")),
    sequence()
  )

  private val expectedBody = sequence(
    str("Apply"),
    expectedIntType,
    sequence(
      str("Apply"),
      expectedIntType,
      sequence(
        str("Reference"),
        expectedIntType,
        fqName(FQName.fqn("Morphir.SDK", "Basics", "add"))
      ),
      sequence(str("Variable"), expectedIntType, name(Name.fromString("left")))
    ),
    sequence(str("Variable"), expectedIntType, name(Name.fromString("right")))
  )

  private val expectedIr = record(
    "formatVersion" -> integer(3),
    "distribution"  -> sequence(
      str("Library"),
      renderPath(packageName.toPath),
      sequence(),
      record(
        "modules" -> sequence(
          sequence(
            renderPath(moduleName.toPath),
            record(
              "access" -> str("Public"),
              "value"  -> record(
                "types"  -> sequence(),
                "values" -> sequence(
                  sequence(
                    name(addName),
                    record(
                      "access" -> str("Public"),
                      "value"  -> record(
                        "doc"   -> str(""),
                        "value" -> record(
                          "inputTypes" -> sequence(
                            sequence(name(Name.fromString("left")), expectedIntType, expectedIntType),
                            sequence(name(Name.fromString("right")), expectedIntType, expectedIntType)
                          ),
                          "outputType" -> expectedIntType,
                          "body"       -> expectedBody
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  "Kyo Structure.Value encodes as natural JSON" in {
    val value = Structure.Value.Record(
      Chunk(
        "name"   -> Structure.Value.Str("example"),
        "values" -> Structure.Value.Sequence(
          Chunk(Structure.Value.Str("value"), Structure.Value.Integer(3))
        )
      )
    )

    assert(Json.encode(value) == """{"name":"example","values":["value",3]}""")
  }

  "projects a Kyo code-model library to the exact typed IR v3 wire tree" in {
    assert(V3WireProjection.project(libraryWith()) == Result.succeed(expectedIr))
    assert(V3WireProjection.encode(libraryWith()) == Result.succeed(Json.encode(expectedIr)))
  }

  "projects wide collections without consuming the JVM stack" in {
    val elements       = Chunk.from(List.fill(50_000)(cm.Expr.Unit(cm.ValueAttributes.empty)))
    val wideDefinition = cm.ValueDefinition(
      cm.AccessControlled(
        cm.Access.Public,
        cm.ValueDefinitionBody.ExpressionBody(
          Chunk.empty,
          intType,
          cm.Expr.List(cm.ValueAttributes.empty, elements)
        )
      )
    )

    V3WireProjection.project(libraryWith(wideDefinition)) match
      case Result.Success(_) => assert(true)
      case other             => assert(false, s"expected a successful wide projection, got $other")
  }

  "projects deeply nested expressions without consuming the JVM stack" in {
    val base   = cm.Expr.Reference(cm.ValueAttributes.empty, FQName.fqn("Morphir.SDK", "Basics", "identity"))
    val nested = List.fill(10_000)(()).foldLeft(base) { (function, _) =>
      cm.Expr.Apply(
        cm.ValueAttributes.empty,
        function,
        cm.Expr.Unit(cm.ValueAttributes.empty)
      )
    }
    val deepDefinition = cm.ValueDefinition(
      cm.AccessControlled(
        cm.Access.Public,
        cm.ValueDefinitionBody.ExpressionBody(Chunk.empty, intType, nested)
      )
    )

    V3WireProjection.project(libraryWith(deepDefinition)) match
      case Result.Success(_) => assert(true)
      case other             => assert(false, s"expected a successful deep projection, got $other")
  }

  "renders decimal literals in schema-compatible plain notation" in {
    val decimalDefinition = cm.ValueDefinition(
      cm.AccessControlled(
        cm.Access.Public,
        cm.ValueDefinitionBody.ExpressionBody(
          Chunk.empty,
          intType,
          cm.Expr.Literal(
            cm.ValueAttributes.empty,
            cm.Literal.DecimalLiteral(BigDecimal("1E+10"))
          )
        )
      )
    )

    V3WireProjection.encode(libraryWith(decimalDefinition)) match
      case Result.Success(json) =>
        assert(json.contains("[\"DecimalLiteral\",\"10000000000\"]"))
      case other => assert(false, s"expected a successful decimal projection, got $other")
  }

  "rejects distribution kinds that Morphir IR v3 cannot represent" in {
    val application = cm.Distribution.Application(
      cm.ApplicationDistribution(
        cm.PackageInfo(packageName, ""),
        cm.PackageDefinition(Map.empty),
        Map.empty,
        Map.empty
      )
    )

    assert(
      V3WireProjection.project(application) ==
        Result.fail(V3ProjectionError.UnsupportedDistribution("Application"))
    )
  }

  "rejects Kyo-native value bodies rather than degrading them" in {
    val native = addDefinition.copy(
      body = cm.AccessControlled(
        cm.Access.Public,
        cm.ValueDefinitionBody.NativeBody(
          Chunk(cm.Parameter(Name.fromString("left"), intType)),
          intType,
          cm.NativeInfo(cm.NativeHint.Arithmetic, None)
        )
      )
    )

    V3WireProjection.project(libraryWith(native)) match
      case Result.Failure(V3ProjectionError.UnsupportedFeature(path, feature)) =>
        assert(path.contains("add"))
        assert(feature == "NativeBody")
      case other => assert(false, s"expected an unsupported native-body failure, got $other")
  }
