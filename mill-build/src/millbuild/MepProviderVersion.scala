package millbuild

object MepProviderVersion:
  val EnvironmentVariable = "MORPHIR_ELM_MEP_VERSION"
  val Default             = "0.1.0"

  def fromEnvironment(environment: Map[String, String]): String =
    environment.get(EnvironmentVariable).filter(_.nonEmpty).getOrElse(Default)
