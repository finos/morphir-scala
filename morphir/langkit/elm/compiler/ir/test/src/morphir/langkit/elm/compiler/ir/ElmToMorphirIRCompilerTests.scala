package morphir.langkit.elm.compiler.ir

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span
import morphir.langkit.elm.ast
import morphir.langkit.elm.ast.ModuleType
import org.finos.morphir.codemodel as cm
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
      moduleName: ModuleName = ModuleName.fromString("Example"),
      exposedValues: Set[Name] = Set(Name.fromString("add"))
  ) = CompileInput(
    source = sourceText,
    packageName = PackageName.fromString("local/example"),
    moduleName = moduleName,
    exposedValues = exposedValues
  )

  "ElmToMorphirIRCompiler" - {
    "compiles a public typed add function into the Kyo code model" in {
      val request = input()

      ElmToMorphirIRCompiler.compile(request) match
        case Result.Success(cm.Distribution.Library(library)) =>
          assert(library.packageInfo.name == request.packageName)
          assert(library.packageInfo.version.isEmpty)
          assert(library.dependencies.isEmpty)

          val module = library.definition.modules(request.moduleName)
          assert(module.access == cm.Access.Public)

          val documented = module.value.values(Name.fromString("add"))
          assert(documented.access == cm.Access.Public)
          assert(documented.value.doc.isEmpty)
          assert(documented.value.value.body.access == cm.Access.Public)

          documented.value.value.body.value match
            case cm.ValueDefinitionBody.ExpressionBody(inputTypes, outputType, body) =>
              assert(inputTypes == Chunk(
                cm.Parameter(Name.fromString("left"), ElmCodeModel.intType),
                cm.Parameter(Name.fromString("right"), ElmCodeModel.intType)
              ))
              assert(outputType == ElmCodeModel.intType)
              body match
                case cm.Expr.Apply(
                      attributes,
                      cm.Expr.Apply(
                        innerAttributes,
                        cm.Expr.Reference(referenceAttributes, reference),
                        cm.Expr.Variable(leftAttributes, left)
                      ),
                      cm.Expr.Variable(rightAttributes, right)
                    ) =>
                  assert(reference == FQName.fqn("Morphir.SDK", "Basics", "add"))
                  assert(left == Name.fromString("left"))
                  assert(right == Name.fromString("right"))
                  assert(
                    Seq(attributes, innerAttributes, referenceAttributes, leftAttributes, rightAttributes)
                      .forall(_.inferredType.contains(ElmCodeModel.intType))
                  )
                case other => assert(false, s"expected curried Morphir SDK addition, got $other")
            case other => assert(false, s"expected an expression body, got $other")
        case other => assert(false, s"expected successful Kyo code-model compilation, got $other")
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

    "recognizes a malformed module header after leading comments" in {
      val malformedHeader = "-- module documentation\nmodule Malformed\n\nadd : Int\nadd = 1\n"

      ElmToMorphirIRCompiler.compile(input(sourceText = malformedHeader)) match
        case Result.Failure(CompileFailure(Chunk(CompileDiagnostic.MalformedModuleHeader(span)))) =>
          assert(malformedHeader.substring(span.start, span.end) == "module Malformed")
        case other => assert(false, s"expected malformed module header after a comment, got $other")
    }

    "recognizes a malformed module header after a leading block comment" in {
      val malformedHeader = "{- lead -}\nmodule Malformed\n\nadd : Int\nadd = 1\n"

      ElmToMorphirIRCompiler.compile(input(sourceText = malformedHeader)) match
        case Result.Failure(CompileFailure(Chunk(CompileDiagnostic.MalformedModuleHeader(span)))) =>
          assert(malformedHeader.substring(span.start, span.end) == "module Malformed")
        case other => assert(false, s"expected malformed module header after a block comment, got $other")
    }

    "recognizes a malformed module header after a leading doc comment" in {
      val malformedHeader = "{-| docs -}\nmodule Malformed\n\nadd : Int\nadd = 1\n"

      ElmToMorphirIRCompiler.compile(input(sourceText = malformedHeader)) match
        case Result.Failure(CompileFailure(Chunk(CompileDiagnostic.MalformedModuleHeader(span)))) =>
          assert(malformedHeader.substring(span.start, span.end) == "module Malformed")
        case other => assert(false, s"expected malformed module header after a doc comment, got $other")
    }

    "recognizes a malformed module header after nested block comments" in {
      val malformedHeader = "{- outer {- inner -} outer -}\nmodule Malformed\n\nadd : Int\nadd = 1\n"

      ElmToMorphirIRCompiler.compile(input(sourceText = malformedHeader)) match
        case Result.Failure(CompileFailure(Chunk(CompileDiagnostic.MalformedModuleHeader(span)))) =>
          assert(malformedHeader.substring(span.start, span.end) == "module Malformed")
        case other => assert(false, s"expected malformed module header after nested block comments, got $other")
    }

    "recognizes a malformed multiline module header" in {
      val malformedHeader =
        "module Example exposing\n    (add\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"

      ElmToMorphirIRCompiler.compile(input(sourceText = malformedHeader)) match
        case Result.Failure(CompileFailure(Chunk(CompileDiagnostic.MalformedModuleHeader(span)))) =>
          assert(malformedHeader.substring(span.start, span.end) == "module Example exposing\n    (add")
        case other => assert(false, s"expected malformed multiline module header, got $other")
    }

    "recognizes balanced malformed multiline exposure syntax" in {
      val malformedHeader =
        "module Example exposing\n    (add,\n    )\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"

      ElmToMorphirIRCompiler.compile(input(sourceText = malformedHeader)) match
        case Result.Failure(CompileFailure(Chunk(CompileDiagnostic.MalformedModuleHeader(span)))) =>
          assert(malformedHeader.substring(span.start, span.end) == "module Example exposing\n    (add,\n    )")
        case other => assert(false, s"expected balanced malformed multiline module header, got $other")
    }

    "keeps an invalid body after a valid multiline header as a parser failure" in {
      val invalidBody =
        "module Example exposing\n    (add\n    )\n\nadd : Int -> Int -> Int\nadd left right =\n"

      ElmToMorphirIRCompiler.compile(input(sourceText = invalidBody)) match
        case Result.Failure(CompileFailure(Chunk(CompileDiagnostic.ParserFailure(diagnostic)))) =>
          assert(diagnostic.span.range.start > invalidBody.indexOf("add left right"))
        case other => assert(false, s"expected body parser failure after a valid multiline header, got $other")
    }

    "keeps a module-prefixed identifier as an ordinary parser failure" in {
      val notAHeader = "moduleName = 1\n"

      ElmToMorphirIRCompiler.compile(input(sourceText = notAHeader)) match
        case Result.Failure(CompileFailure(Chunk(error @ CompileDiagnostic.ParserFailure(_)))) =>
          assert(error.code == "ELM-IR001")
        case other => assert(false, s"expected ordinary parser failure for a module-prefixed identifier, got $other")
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

    "parses a valid effect module header before rejecting its module type" in {
      val effectModule = source.replace(
        "module Example exposing (add)",
        "effect module Example where { command = MyCmd } exposing (add)"
      )

      ElmToMorphirIRCompiler.compile(input(sourceText = effectModule)) match
        case Result.Failure(CompileFailure(Chunk(CompileDiagnostic.UnsupportedModule(moduleType, _)))) =>
          assert(moduleType == ModuleType.Effect)
        case other => assert(false, s"expected unsupported effect module failure, got $other")
    }

    "compiles a valid multiline exposing header" in {
      val multilineHeader = source.replace(
        "module Example exposing (add)",
        """module Example exposing
          |    ( add
          |    )""".stripMargin
      )

      ElmToMorphirIRCompiler.compile(input(sourceText = multilineHeader)) match
        case Result.Success(_: cm.Distribution.Library) => succeed
        case other => assert(false, s"expected successful multiline-header compilation, got $other")
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

    "rejects deeply nested unsupported Elm types without consuming the JVM stack" in {
      val intSpan      = Span(0, 3)
      val floatSpan    = Span(40_000, 5)
      val intType      = ast.TypeReference(ast.QualifiedName(List("Int"))(intSpan))(intSpan)
      val floatType    = ast.TypeReference(ast.QualifiedName(List("Float"))(floatSpan))(floatSpan)
      val deeplyNested = List.fill(10_000)(()).foldLeft[ast.TypeExpression](floatType) { (result, _) =>
        ast.FunctionType(intType, result)(Span(intSpan.offset, result.span.end - intSpan.offset))
      }

      ElmToMorphirIRCompiler.unsupportedTypeExpression(deeplyNested) match
        case Some((kind, span)) =>
          assert(kind == "Float")
          assert(span == floatSpan)
        case other => assert(false, s"expected deep unsupported type failure, got $other")
    }

    "rejects an annotation bound to a different declaration name" in {
      val mismatchedAnnotation = source.replace("add : Int -> Int -> Int", "other : Int -> Int -> Int")

      ElmToMorphirIRCompiler.compile(input(sourceText = mismatchedAnnotation)) match
        case Result.Failure(
              CompileFailure(
                Chunk(error @ CompileDiagnostic.AnnotationNameMismatch(annotationName, declarationName, span))
              )
            ) =>
          assert(error.code == "ELM-IR013")
          assert(annotationName == "other")
          assert(declarationName == "add")
          assert(span.length == "other".length)
        case other => assert(false, s"expected annotation name mismatch failure, got $other")
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

    "rejects duplicate variable parameters at the duplicate span" in {
      val duplicateParameter = source.replace("add left right =", "add left left =").replace(
        "left + right",
        "left + left"
      )

      ElmToMorphirIRCompiler.compile(input(sourceText = duplicateParameter)) match
        case Result.Failure(CompileFailure(Chunk(error @ CompileDiagnostic.DuplicateParameter(name, span)))) =>
          assert(error.code == "ELM-IR014")
          assert(name == "left")
          assert(duplicateParameter.substring(span.start, span.end) == "left")
        case other => assert(false, s"expected duplicate parameter failure, got $other")
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

    "rejects duplicate exposed values at the duplicate span" in {
      val duplicateExposure = source.replace("exposing (add)", "exposing (add, add)")

      ElmToMorphirIRCompiler.compile(input(sourceText = duplicateExposure)) match
        case Result.Failure(CompileFailure(Chunk(error @ CompileDiagnostic.DuplicateExposedValue(name, span)))) =>
          assert(error.code == "ELM-IR015")
          assert(name == Name.fromString("add"))
          assert(duplicateExposure.substring(span.start, span.end) == "add")
        case other => assert(false, s"expected duplicate exposed value failure, got $other")
    }

    "rejects exposed operators in the explicit-value-only slice" in {
      val exposedOperator = source.replace("exposing (add)", "exposing (add, (+))")

      ElmToMorphirIRCompiler.compile(input(sourceText = exposedOperator)) match
        case Result.Failure(CompileFailure(Chunk(error @ CompileDiagnostic.UnsupportedExposure(kind, span)))) =>
          assert(error.code == "ELM-IR012")
          assert(kind == "operator +")
          assert(span.length > 0)
        case other => assert(false, s"expected unsupported exposure failure, got $other")
    }

    "rejects exposed types in the explicit-value-only slice" in {
      val exposedType = source.replace("exposing (add)", "exposing (add, Thing)")

      ElmToMorphirIRCompiler.compile(input(sourceText = exposedType)) match
        case Result.Failure(CompileFailure(Chunk(CompileDiagnostic.UnsupportedExposure(kind, span)))) =>
          assert(kind == "type Thing")
          assert(span.length > 0)
        case other => assert(false, s"expected unsupported exposure failure, got $other")
    }

    "rejects expose-all in the explicit-value-only slice" in {
      val exposeAll = source.replace("exposing (add)", "exposing (..)")

      ElmToMorphirIRCompiler.compile(input(sourceText = exposeAll)) match
        case Result.Failure(CompileFailure(Chunk(CompileDiagnostic.UnsupportedExposure(kind, span)))) =>
          assert(kind == "all values")
          assert(span.length > 0)
        case other => assert(false, s"expected unsupported exposure failure, got $other")
    }
  }
