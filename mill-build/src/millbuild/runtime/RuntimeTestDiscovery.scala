package millbuild.runtime

object RuntimeTestDiscovery {
  val requiredClassNames: Set[String] = Set(
    "org.finos.morphir.runtime.DefaultsSpec",
    "org.finos.morphir.runtime.EvaluatorMDMTests",
    "org.finos.morphir.runtime.EvaluatorQuickSpec",
    "org.finos.morphir.runtime.quick.GatherRefsSpec",
    "org.finos.morphir.runtime.TypeCheckerTests",
    "org.finos.morphir.runtime.UnitTestingSpec",
    "org.finos.morphir.runtime.parsing.ParseSpec"
  )

  def missing(discoveredClassNames: Seq[String]): Seq[String] =
    (requiredClassNames -- discoveredClassNames).toSeq.sorted

  def requireAllDiscovered(discoveredClassNames: Seq[String]): Unit =
    missing(discoveredClassNames) match {
      case Seq() => ()
      case absent =>
        throw new IllegalStateException(
          s"Missing required classic runtime test classes: ${absent.mkString(", ")}"
        )
    }
}
