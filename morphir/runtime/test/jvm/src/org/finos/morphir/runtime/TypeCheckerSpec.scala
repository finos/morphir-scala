package org.finos.morphir.runtime

import org.finos.morphir.testing.MorphirBaseSpec
import zio.{Console, Task, ZIO, ZLayer}
import zio.test.{test, *}
import org.finos.morphir.runtime.MorphirRuntime
import org.finos.morphir.datamodel.{Data, Concept, Label, EnumLabel}
import org.finos.morphir.datamodel.namespacing.Namespace.ns
import org.finos.morphir.datamodel.namespacing.PackageName.root
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.ir.Type
import org.finos.morphir.naming.*
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.environment.MorphirEnv
import zio.test.TestAspect.{ignore, tag}

object TypeCheckerSpec extends MorphirBaseSpec {
  type MorphirRuntimeTyped = MorphirRuntime[Unit, Type.UType]

  val morphirRuntimeLayer: ZLayer[Any, Throwable, MorphirRuntime[Unit, Type.UType]] =
    ZLayer(for {
      irFilePath <- ZIO.succeed(os.pwd / "examples" / "morphir-elm-projects" / "evaluator-tests" / "morphir-ir.json")
      _          <- Console.printLine(s"Loading distribution from $irFilePath")
      dist       <- EvaluationLibrary.loadDistributionFromFileZIO(irFilePath.toString)
    } yield MorphirRuntime.quick(dist))

  def deriveData(input: Any): Data =
    input match {
      case u: Unit          => Deriver.toData(u)
      case i: Int           => Deriver.toData(i)
      case s: String        => Deriver.toData(s)
      case (l: Any, r: Any) => Data.Tuple(deriveData(l), deriveData(r))
      case l: List[_]       => Data.List(deriveData(l.head), l.tail.map(deriveData(_)): _*)
      case other            => throw new Exception(s"Couldn't derive $other")
    }

  def checkEvaluation(
      moduleName: String,
      functionName: String,
      value: Any
  )(expected: => Data): ZIO[MorphirRuntimeTyped, Throwable, TestResult] =
    runTest(moduleName, functionName, value).map { actual =>
      assertTrue(actual == expected)
    }
  def testEval(label: String)(moduleName: String, functionName: String, value: Any)(expected: => Data) =
    test(label) {
      checkEvaluation(moduleName, functionName, value)(expected)
    }
  def runTest(
      moduleName: String,
      functionName: String,
      value: Any
  ): ZIO[MorphirRuntimeTyped, Throwable, Data] =
    ZIO.serviceWithZIO[MorphirRuntimeTyped] { runtime =>
      val fullName = s"Morphir.Examples.App:$moduleName:$functionName"
      val data     = deriveData(value)

      runtime.evaluate(FQName.fromString(fullName), data)
        .provideEnvironment(MorphirEnv.live)
        .toZIOWith(RTExecutionContext.default)
    }

  def typeArgUnionShape(c1: Concept, c2: Concept): Concept.Enum = Concept.Enum(
    qn"Morphir/Examples/App:ExampleModule:TypeArgUnion",
    List(
      Concept.Enum.Case(
        Label("b"),
        List(Tuple2(
          EnumLabel.Named("arg1"),
          c2
        ))
      ),
      Concept.Enum.Case(
        Label("aB"),
        List(
          (
            EnumLabel.Named("arg1"),
            c1
          ),
          (
            EnumLabel.Named("arg2"),
            c2
          )
        )
      ),
      Concept.Enum.Case(
        Label("a"),
        List((
          EnumLabel.Named("arg1"),
          c1
        ))
      ),
      Concept.Enum.Case(
        Label("dictBA"),
        List((
          EnumLabel.Named("arg1"),
          Concept.Map(
            c2,
            c1
          )
        ))
      ),
      Concept.Enum.Case(
        Label("maybeA"),
        List((
          EnumLabel.Named("arg1"),
          Concept.Optional(c1)
        ))
      )
    )
  )

  def spec =
    suite("Evaluator Type Checker Tests")(
      suite("List Tests")(
        testEval("Finds element type of list")("typeCheckerTests", "withParam", List(4, 5, 7))(Data.Int(4))
      )
    ).provideLayerShared(morphirRuntimeLayer)
}
