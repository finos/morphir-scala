package org.finos.morphir.codemodel

import kyo.test.*
import kyo.{Chunk, Result}
import kyo.Json.given_Json
import org.finos.morphir.naming.*

/**
 * Round-trip coverage for `CodeModelCodecs`, the derived replacement for the hand-written
 * `MorphirJsonEncodingSupportV4`/`DecodingSupportV4`. The v4 wire format is new and unshipped, so whatever kyo-schema
 * derives here is the format — there is no legacy encoding to reproduce.
 *
 * The maps below are deliberately NON-EMPTY and keyed by `PackageName` / `ModuleName` / `Name`. Empty maps would not
 * exercise non-string map-key encoding, which is the likeliest way these derived instances (all 15 of them, per the
 * Task 3 review) could silently fail to round-trip.
 */
class CodeModelCodecsSpec extends Test[Any]:

  private val basicsModuleName = ModuleName.fromString("Basics")

  private val moduleSpec: ModuleSpecification =
    ModuleSpecification(
      types = Map(
        Name.fromString("maybe") -> Documented(
          Some(Documentation(Chunk("Represents an optional value."))),
          TypeSpecification.OpaqueTypeSpecification(Chunk(Name.fromString("a")))
        )
      ),
      values = Map(
        Name.fromString("add") -> Documented(
          None,
          ValueSpecification(
            inputs = Chunk(Parameter(Name.fromString("x"), Type.Variable(TypeAttributes.empty, Name.fromString("a")))),
            output = Type.Variable(TypeAttributes.empty, Name.fromString("a"))
          )
        )
      )
    )

  private val dep: PackageSpecification =
    PackageSpecification(Map(basicsModuleName -> moduleSpec))

  private val specsDist: Distribution =
    Distribution.Specs(
      SpecsDistribution(
        packageInfo = PackageInfo(PackageName.fromString("Morphir.SDK"), "1.0.0"),
        specification = dep,
        dependencies = Map(PackageName.fromString("Morphir.Base") -> dep)
      )
    )

  "round-trips a Distribution.Specs, including non-string map keys" in {
    val json = CodeModelCodecs.encodeDistribution(specsDist)
    assert(CodeModelCodecs.decodeDistribution(json) == Result.succeed(specsDist))
  }

  // A PackageDefinition with an actual module (types and values), exercising
  // AccessControlled/Documented/TypeDefinition/ValueDefinition round-tripping, none of which
  // SchemaDerivationSpec or ValSpec touch.
  private val moduleDef: ModuleDefinition =
    ModuleDefinition(
      types = Map(
        Name.fromString("maybe") -> AccessControlled(
          Access.Public,
          Documented(
            Some(Documentation(Chunk("Represents an optional value."))),
            TypeDefinition.CustomTypeDefinition(
              params = Chunk(Name.fromString("a")),
              access = AccessControlled(
                Access.Public,
                Chunk(
                  Constructor(
                    Name.fromString("Just"),
                    Chunk(Parameter(
                      Name.fromString("value"),
                      Type.Variable(TypeAttributes.empty, Name.fromString("a"))
                    ))
                  ),
                  Constructor(Name.fromString("Nothing"), Chunk.empty)
                )
              )
            )
          )
        )
      ),
      values = Map(
        Name.fromString("add") -> AccessControlled(
          Access.Public,
          Documented(
            None,
            ValueDefinition(
              AccessControlled(
                Access.Public,
                ValueDefinitionBody.ExpressionBody(
                  inputTypes =
                    Chunk(Parameter(Name.fromString("x"), Type.Variable(TypeAttributes.empty, Name.fromString("a")))),
                  outputType = Type.Variable(TypeAttributes.empty, Name.fromString("a")),
                  body = Expr.Variable(ValueAttributes.empty, Name.fromString("x"))
                )
              )
            )
          )
        )
      )
    )

  private val packageDef: PackageDefinition =
    PackageDefinition(Map(basicsModuleName -> AccessControlled(Access.Public, moduleDef)))

  private val libraryDist: Distribution =
    Distribution.Library(
      LibraryDistribution(
        packageInfo = PackageInfo(PackageName.fromString("Morphir.SDK"), "1.0.0"),
        definition = packageDef,
        dependencies = Map(PackageName.fromString("Morphir.Base") -> dep)
      )
    )

  "round-trips a Distribution.Library with a module containing types and values" in {
    val json = CodeModelCodecs.encodeDistribution(libraryDist)
    assert(CodeModelCodecs.decodeDistribution(json) == Result.succeed(libraryDist))
  }

  private val applicationDist: Distribution =
    Distribution.Application(
      ApplicationDistribution(
        packageInfo = PackageInfo(PackageName.fromString("Morphir.SDK"), "1.0.0"),
        definition = packageDef,
        dependencies = Map(PackageName.fromString("Morphir.Base") -> packageDef),
        entryPoints = Map(
          Name.fromString("main") -> EntryPoint(
            target = FQName.fromString("Morphir.SDK:Basics:add"),
            kind = EntryPointKind.Main,
            doc = None
          )
        )
      )
    )

  "round-trips a Distribution.Application, including Name-keyed entry points" in {
    val json = CodeModelCodecs.encodeDistribution(applicationDist)
    assert(CodeModelCodecs.decodeDistribution(json) == Result.succeed(applicationDist))
  }

end CodeModelCodecsSpec
