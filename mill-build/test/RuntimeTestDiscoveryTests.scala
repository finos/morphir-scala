//| moduleDeps: ["//mill-build/src/millbuild/runtime/RuntimeTestDiscovery.scala"]

import millbuild.runtime.RuntimeTestDiscovery

def assertEquals[A](actual: A, expected: A): Unit =
  assert(actual == expected, s"Expected $expected, got $actual")

@main def runRuntimeTestDiscoveryTests(): Unit = {
  val required = Set(
    "org.finos.morphir.runtime.DefaultsSpec",
    "org.finos.morphir.runtime.EvaluatorMDMTests",
    "org.finos.morphir.runtime.EvaluatorQuickSpec",
    "org.finos.morphir.runtime.quick.GatherRefsSpec",
    "org.finos.morphir.runtime.TypeCheckerTests",
    "org.finos.morphir.runtime.UnitTestingSpec",
    "org.finos.morphir.runtime.parsing.ParseSpec"
  )

  assertEquals(RuntimeTestDiscovery.requiredClassNames, required)
  assertEquals(RuntimeTestDiscovery.missing(required.toSeq :+ "example.UnrelatedSpec"), Seq.empty)
  assertEquals(
    RuntimeTestDiscovery.missing(required.toSeq.filterNot(_.endsWith("DefaultsSpec"))),
    Seq("org.finos.morphir.runtime.DefaultsSpec")
  )

  val error = try {
    RuntimeTestDiscovery.requireAllDiscovered(Seq("org.finos.morphir.runtime.EvaluatorMDMTests"))
    throw new AssertionError("Expected incomplete discovery to fail")
  } catch {
    case error: IllegalStateException => error
  }
  assert(error.getMessage.contains("Missing required classic runtime test classes:"))
  assert(error.getMessage.contains("org.finos.morphir.runtime.DefaultsSpec"))
  assert(error.getMessage.contains("org.finos.morphir.runtime.parsing.ParseSpec"))
}
