package millbuild

object MepProviderVersion:
  val EnvironmentVariable = "MORPHIR_ELM_MEP_VERSION"
  val Default             = morphir.langkit.elm.compiler.mep.BuildExtensionDefinition.DefaultVersion

  def fromEnvironment(environment: Map[String, String]): String =
    environment.get(EnvironmentVariable).filter(_.nonEmpty).getOrElse(Default)
