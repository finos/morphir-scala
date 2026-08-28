package morphir.langkit.elm.compiler.mep

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span as SourceSpan
import morphir.langkit.elm.compiler.ir.{CompileDiagnostic, CompileInput, ElmToMorphirIRCompiler}
import org.finos.morphir.ir.{MorphirIRFile, MorphirIRVersion}
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.naming.{ModuleName, Name, PackageName}
import zio.json.ast.Json

class MepElmFrontendTests extends Test[Any]:
  private val packageName = PackageName.fromString("local/example")
  private val moduleName  = ModuleName.fromString("Example")
  private val source      =
    """module Example exposing (add)
      |
      |add : Int -> Int -> Int
      |add left right = left + right
      |""".stripMargin

  private val validIr = ElmToMorphirIRCompiler.compile(
    CompileInput(source, packageName, moduleName, Set(Name.fromString("add")), MorphirIRVersion.V3_0)
  ) match
    case Result.Success(ir) => ir
    case other              => throw AssertionError(s"test fixture did not compile: $other")

  "MepCompileError" - {
    "classifies semantic request errors as invalid compile parameters" in {
      val result = MepElmFrontend.compile(Json.Obj())

      assert(result == Left(MepCompileError.InvalidParams("languageId must be a string")))
    }

    "maps invalid compile parameters to JSON-RPC invalid params" in {
      val code = MepCompileError.jsonRpcCode(MepCompileError.InvalidParams("bad params"))

      assert(code == -32602)
    }

    "maps invalid compiler output to JSON-RPC internal error" in {
      val code = MepCompileError.jsonRpcCode(MepCompileError.InvalidCompilerOutput("bad IR"))

      assert(code == -32603)
    }

    "maps IR serialization failure to JSON-RPC internal error" in {
      val code = MepCompileError.jsonRpcCode(MepCompileError.IRSerializationFailure("bad JSON"))

      assert(code == -32603)
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

  "MepElmFrontend.validateCompiledIR" - {
    "classifies invalid constructed IR as invalid compiler output" in {
      val result = MepElmFrontend.validateCompilerOutput(
        validIr.copy(version = MorphirIRVersion.V2_0),
        packageName,
        Set(moduleName)
      )

      assert(result ==
        Left(MepCompileError.InvalidCompilerOutput("The compiler returned Morphir IR other than version 3")))
    }

    "rejects invalid constructed IR before encoding a successful compile result" in {
      val result = MepElmFrontend.encodeCompilerOutput(
        validIr.copy(version = MorphirIRVersion.V2_0),
        packageName,
        Set(moduleName)
      )

      assert(result ==
        Left(MepCompileError.InvalidCompilerOutput("The compiler returned Morphir IR other than version 3")))
    }

    "derives the package and module metadata from valid IR" in {
      val validated = MepElmFrontend.validateCompiledIR(validIr, packageName, Set(moduleName)).toOption.get

      assert(validated.ir == validIr)
      assert(validated.packageName == packageName)
      assert(validated.modules.toSet == Set(moduleName))
    }

    "rejects a compiled IR version other than v3" in {
      val result = MepElmFrontend.validateCompiledIR(
        validIr.copy(version = MorphirIRVersion.V2_0),
        packageName,
        Set(moduleName)
      )

      assert(result == Left("The compiler returned Morphir IR other than version 3"))
    }

    "rejects a compiled IR distribution that is not a library" in {
      val result = MepElmFrontend.validateCompiledIR(
        validIr.copy(distribution = Distribution.Bundle(Map.empty)),
        packageName,
        Set(moduleName)
      )

      assert(result == Left("The compiler returned a non-library distribution"))
    }

    "rejects a compiled library whose package differs from the request" in {
      val wrongPackage = PackageName.fromString("local/other")
      val library      = validIr.distribution match
        case value: Distribution.Library => value
        case other                       => throw AssertionError(s"expected library fixture, got $other")
      val result = MepElmFrontend.validateCompiledIR(
        validIr.copy(distribution = library.copy(packageName = wrongPackage)),
        packageName,
        Set(moduleName)
      )

      assert(result == Left("The compiled IR package does not match the requested package"))
    }

    "rejects compiled IR modules that differ from requested module metadata" in {
      val result = MepElmFrontend.validateCompiledIR(
        validIr,
        packageName,
        Set(ModuleName.fromString("Other"))
      )

      assert(result == Left("The compiled IR modules do not match the requested modules"))
    }
  }
