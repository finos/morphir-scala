package morphir.langkit.elm.compiler.ir

import kyo.*
import kyo.test.*
import morphir.langkit.elm.ast.ModuleType
import org.finos.morphir.ir.*
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.naming.*

class ElmToMorphirIRCompilerTests extends Test[Any]:

  private val source =
    """module Example exposing (add)
      |
      |add : Int -> Int -> Int
      |add left right =
      |    left + right
      |""".stripMargin

  private def input(
      sourceText: String = source,
      version: MorphirIRVersion = MorphirIRVersion.V3_0,
      moduleName: ModuleName = ModuleName.fromString("Example"),
      exposedValues: Set[Name] = Set(Name.fromString("add"))
  ) = CompileInput(
    source = sourceText,
    packageName = PackageName.fromString("local/example"),
    moduleName = moduleName,
    exposedValues = exposedValues,
    irVersion = version
  )

  "ElmToMorphirIRCompiler" - {
    "compiles a public typed add function into classic IR v3" in {
      val request = input()

      ElmToMorphirIRCompiler.compile(request) match
        case Result.Success(MorphirIRFile(MorphirIRVersion.V3_0, library: Distribution.Library)) =>
          assert(library.packageName == request.packageName)
          assert(library.dependencies.isEmpty)

          val module = library.packageDef.modules(request.moduleName)
          assert(module.withPublicAccess.isDefined)

          val documented = module.withPrivateAccess.values(Name.fromString("add"))
          assert(documented.withPublicAccess.isDefined)
          val definition = documented.withPrivateAccess.value
          assert(
            definition.inputTypes.map { case (name, attributes, tpe) => (name, attributes, tpe) }.toList == List(
              (Name.fromString("left"), Basics.intType, Basics.intType),
              (Name.fromString("right"), Basics.intType, Basics.intType)
            )
          )
          assert(definition.outputType == Basics.intType)

          val expectedBody = Value.applyInferType(
            Basics.intType,
            Basics.add,
            Value.variable("left", Basics.intType),
            Value.variable("right", Basics.intType)
          )
          assert(definition.body == expectedBody)
          assert(definition.body.attributes == Basics.intType)
          assert(definition.body.collectReferences == Set(FQName.fqn("Morphir.SDK", "Basics", "add")))
        case other => assert(false, s"expected successful classic IR v3 compilation, got $other")
    }

    "rejects an unsupported IR version before producing IR" in {
      ElmToMorphirIRCompiler.compile(input(version = MorphirIRVersion.V4_0)) match
        case Result.Failure(CompileFailure(Chunk(CompileDiagnostic.UnsupportedIRVersion(version)))) =>
          assert(version == MorphirIRVersion.V4_0)
        case other => assert(false, s"expected unsupported IR version failure, got $other")
    }

    "preserves the parser diagnostic and source span" in {
      val malformed = "module Example exposing (add)\n\nadd : Int\nadd =\n"

      ElmToMorphirIRCompiler.compile(input(sourceText = malformed)) match
        case Result.Failure(CompileFailure(Chunk(error @ CompileDiagnostic.ParserFailure(diagnostic)))) =>
          assert(error.code == "ELM-IR001")
          assert(diagnostic.span.range.start > 0)
          assert(diagnostic.span.range.end >= diagnostic.span.range.start)
        case other => assert(false, s"expected parser failure with its diagnostic span, got $other")
    }

    "reports a malformed module header separately from parser failures" in {
      val malformedHeader = "module exposing (add)\n\nadd : Int\nadd = 1\n"

      ElmToMorphirIRCompiler.compile(input(sourceText = malformedHeader)) match
        case Result.Failure(CompileFailure(Chunk(error @ CompileDiagnostic.MalformedModuleHeader(span)))) =>
          assert(error.code == "ELM-IR002")
          assert(span.start == 0)
          assert(span.end == "module exposing (add)".length)
        case other => assert(false, s"expected malformed module header failure, got $other")
    }

    "rejects non-plain Elm modules" in {
      val portModule = "port module Example exposing (add)\n\nport add : Int -> Int\n"

      ElmToMorphirIRCompiler.compile(input(sourceText = portModule)) match
        case Result.Failure(CompileFailure(Chunk(error @ CompileDiagnostic.UnsupportedModule(moduleType, span)))) =>
          assert(error.code == "ELM-IR003")
          assert(moduleType == ModuleType.Port)
          assert(span.length > 0)
        case other => assert(false, s"expected unsupported module failure, got $other")
    }

    "rejects imports instead of guessing dependency IR" in {
      val imported = source.replace("\n\nadd", "\n\nimport Other\n\nadd")

      ElmToMorphirIRCompiler.compile(input(sourceText = imported)) match
        case Result.Failure(CompileFailure(Chunk(error @ CompileDiagnostic.UnsupportedImport(moduleName, span)))) =>
          assert(error.code == "ELM-IR004")
          assert(moduleName == "Other")
          assert(span.length >= "import Other".length)
        case other => assert(false, s"expected unsupported import failure, got $other")
    }

    "rejects unsupported declarations" in {
      val customType = source.replace("\n\nadd", "\n\ntype Thing = Thing\n\nadd")

      ElmToMorphirIRCompiler.compile(input(sourceText = customType)) match
        case Result.Failure(CompileFailure(Chunk(error @ CompileDiagnostic.UnsupportedDeclaration(kind, span)))) =>
          assert(error.code == "ELM-IR005")
          assert(kind == "custom type")
          assert(span.length > 0)
        case other => assert(false, s"expected unsupported declaration failure, got $other")
    }

    "rejects unsupported Elm types" in {
      val floatType = source.replace("add : Int -> Int -> Int", "add : Float -> Int -> Int")

      ElmToMorphirIRCompiler.compile(input(sourceText = floatType)) match
        case Result.Failure(CompileFailure(Chunk(error @ CompileDiagnostic.UnsupportedType(kind, span)))) =>
          assert(error.code == "ELM-IR006")
          assert(kind == "Float")
          assert(span.length >= "Float".length)
        case other => assert(false, s"expected unsupported type failure, got $other")
    }

    "rejects unsupported expressions" in {
      val subtraction = source.replace("left + right", "left - right")

      ElmToMorphirIRCompiler.compile(input(sourceText = subtraction)) match
        case Result.Failure(CompileFailure(Chunk(error @ CompileDiagnostic.UnsupportedExpression(kind, span)))) =>
          assert(error.code == "ELM-IR007")
          assert(kind == "binary operator -")
          assert(span.length >= "left - right".length)
        case other => assert(false, s"expected unsupported expression failure, got $other")
    }

    "rejects unsupported parameter patterns" in {
      val tuplePattern = source.replace("add left right =", "add (left, other) right =")

      ElmToMorphirIRCompiler.compile(input(sourceText = tuplePattern)) match
        case Result.Failure(CompileFailure(Chunk(error @ CompileDiagnostic.UnsupportedPattern(kind, span)))) =>
          assert(error.code == "ELM-IR008")
          assert(kind == "tuple")
          assert(span.length >= "(left, other)".length)
        case other => assert(false, s"expected unsupported pattern failure, got $other")
    }

    "rejects a module name that differs from the caller context" in {
      val differentModule = source.replace("module Example", "module Actual")

      ElmToMorphirIRCompiler.compile(input(sourceText = differentModule)) match
        case Result.Failure(
              CompileFailure(Chunk(error @ CompileDiagnostic.ModuleNameMismatch(expected, actual, span)))
            ) =>
          assert(error.code == "ELM-IR009")
          assert(expected == ModuleName.fromString("Example"))
          assert(actual == ModuleName.fromString("Actual"))
          assert(span.length >= "Actual".length)
        case other => assert(false, s"expected module name mismatch failure, got $other")
    }

    "rejects exposed values that differ from the caller context" in {
      val differentExposure = source.replace("exposing (add)", "exposing (subtract)")

      ElmToMorphirIRCompiler.compile(input(sourceText = differentExposure)) match
        case Result.Failure(
              CompileFailure(Chunk(error @ CompileDiagnostic.ExposedNameMismatch(expected, actual, span)))
            ) =>
          assert(error.code == "ELM-IR010")
          assert(expected == Set(Name.fromString("add")))
          assert(actual == Set(Name.fromString("subtract")))
          assert(span.length >= "subtract".length)
        case other => assert(false, s"expected exposed name mismatch failure, got $other")
    }
  }
