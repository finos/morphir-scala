package morphir.langkit.elm.compiler.mep

import kyo.*
import kyo.test.*
import morphir.langkit.elm.compiler.ir.{CompileInput, ElmToMorphirIRCompiler}
import org.finos.morphir.ir.{MorphirIRFile, MorphirIRVersion}
import org.finos.morphir.ir.distribution.Distribution
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

  private val validIr = ElmToMorphirIRCompiler.compile(
    CompileInput(source, packageName, moduleName, Set(Name.fromString("add")), MorphirIRVersion.V3_0)
  ) match
    case Result.Success(ir) => ir
    case other              => throw AssertionError(s"test fixture did not compile: $other")

  "MepElmFrontend.validateCompiledIR" - {
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
