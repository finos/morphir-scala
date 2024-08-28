package mill.local.plugins.ci.release

private[release] object Discover {
  implicit def millEvaluatorTokenReader = mill.main.TokenReaders.millEvaluatorTokenReader
}