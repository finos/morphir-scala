package morphir.langkit.elm.compiler.mep

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span as SourceSpan
import morphir.langkit.elm.compiler.ir.{CompileDiagnostic, CompileFailure, CompileInput, ElmToMorphirIRCompiler}
import org.finos.morphir.codemodel as cm
import org.finos.morphir.naming.{ModuleName, Name, PackageName}

class MepElmFrontendTests extends Test[Any]:
  private val packageName = PackageName.fromString("local/example")
  private val moduleName  = ModuleName.fromString("Example")
  private val source      =
    """module Example exposing (add)
      |
      |add : Int -> Int -> Int
      |add left right = left + right
      |""".stripMargin

  private val validModel = ElmToMorphirIRCompiler.compile(
    CompileInput(source, packageName, moduleName, Set(Name.fromString("add")))
  ) match
    case Result.Success(model) => model
    case other                 => throw AssertionError(s"test fixture did not compile: $other")

  private def field(value: Structure.Value, name: String): Option[Structure.Value] = value match
    case Structure.Value.Record(fields) => fields.iterator.toMap.get(name)
    case _                              => None

  "MepCompileError" - {
    "classifies malformed Kyo values as invalid compile parameters" in {
      val result = MepElmFrontend.compile(Structure.Value.Record(Chunk.empty))

      assert(result match
        case Result.Failure(_: MepCompileError.InvalidParams) => true
        case _                                                => false)
    }

    "maps request and projection errors to their JSON-RPC categories" in {
      assert(MepCompileError.jsonRpcCode(MepCompileError.InvalidParams("bad params")) == -32602)
      assert(MepCompileError.jsonRpcCode(MepCompileError.InvalidCompilerOutput("bad model")) == -32603)
      assert(MepCompileError.jsonRpcCode(MepCompileError.IRSerializationFailure("bad projection")) == -32603)
    }
  }

  "MepElmFrontend.diagnosticMessage" - {
    "uses stable human-readable text without exposing source span internals" in {
      val diagnostics = Vector(
        CompileDiagnostic.UnsupportedImport("Html", SourceSpan.zero)       -> "Elm imports are not supported: Html",
        CompileDiagnostic.UnsupportedExpression("lambda", SourceSpan.zero) -> "Unsupported Elm expression: lambda",
        CompileDiagnostic.DuplicateParameter("value", SourceSpan.zero)     -> "Duplicate Elm parameter: value"
      )
      val messages = diagnostics.map((diagnostic, _) => MepElmFrontend.diagnosticMessage(diagnostic))

      assert(messages == diagnostics.map(_._2))
      assert(messages.forall(message => !message.contains("Span")))
    }
  }

  "MepElmFrontend.foldCompilerResult" - {
    "maps success and expected failure while preserving panic" in {
      val document = SourceDocument("file:///workspace/Example.elm", "elm", DocumentVersion(1), source)
      val failure  = CompileFailure(Chunk(CompileDiagnostic.UnsupportedExpression("lambda", SourceSpan.zero)))
      val panic    = IllegalStateException("synthetic compiler panic")

      val succeeded = MepElmFrontend.foldCompilerResult(
        document,
        packageName,
        moduleName,
        Chunk("Example"),
        Result.succeed(validModel)
      )
      val failed = MepElmFrontend.foldCompilerResult(
        document,
        packageName,
        moduleName,
        Chunk("Example"),
        Result.fail(failure)
      )
      val panicked = MepElmFrontend.foldCompilerResult(
        document,
        packageName,
        moduleName,
        Chunk("Example"),
        Result.panic(panic)
      )

      assert(succeeded match
        case Result.Success(value) => field(value, "success").contains(Structure.Value.Bool(true))
        case _                     => false)
      assert(failed match
        case Result.Success(value) => field(value, "success").contains(Structure.Value.Bool(false))
        case _                     => false)
      assert(panicked match
        case Result.Panic(cause) => cause eq panic
        case _                   => false)
    }
  }

  "MepElmFrontend.validateCompiledModel" - {
    "derives package and module metadata from the Kyo code model" in {
      val validated = MepElmFrontend.validateCompiledModel(validModel, packageName, Set(moduleName)).toOption.get

      assert(validated.distribution == validModel)
      assert(validated.packageName == packageName)
      assert(validated.modules.toSet == Set(moduleName))
    }

    "rejects a non-library distribution" in {
      val library = validModel match
        case cm.Distribution.Library(value) => value
        case other                          => throw AssertionError(s"expected library fixture, got $other")
      val specs = cm.Distribution.Specs(
        cm.SpecsDistribution(library.packageInfo, cm.PackageSpecification(Map.empty), Map.empty)
      )

      assert(
        MepElmFrontend.validateCompiledModel(specs, packageName, Set(moduleName)) ==
          Left("The compiler returned a non-library distribution")
      )
    }

    "rejects package and module metadata that differ from the request" in {
      val library = validModel match
        case cm.Distribution.Library(value) => value
        case other                          => throw AssertionError(s"expected library fixture, got $other")
      val wrongPackage = cm.Distribution.Library(
        library.copy(packageInfo = library.packageInfo.copy(name = PackageName.fromString("local/other")))
      )

      assert(
        MepElmFrontend.validateCompiledModel(wrongPackage, packageName, Set(moduleName)) ==
          Left("The compiled model package does not match the requested package")
      )
      assert(
        MepElmFrontend.validateCompiledModel(validModel, packageName, Set(ModuleName.fromString("Other"))) ==
          Left("The compiled model modules do not match the requested modules")
      )
    }

    "projects valid compiler output to an embedded Morphir IR v3 value" in {
      val encoded = MepElmFrontend.encodeCompilerOutput(validModel, packageName, Set(moduleName), Chunk("Example"))

      assert(encoded.toOption.flatMap(field(_, "success")).contains(Structure.Value.Bool(true)))
      assert(encoded.toOption.flatMap(field(_, "irVersion")).contains(Structure.Value.Str("3")))
      assert(encoded.toOption.flatMap(field(_, "ir")).exists(_.isInstanceOf[Structure.Value.Record]))
    }
  }
end MepElmFrontendTests
