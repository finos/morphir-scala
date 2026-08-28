package morphir.langkit.elm.compiler.mep

object Main:
  def main(args: Array[String]): Unit =
    val exitCode = MepProcess.run(System.in, System.out, System.err, ProviderMetadata.default)
    if exitCode != 0 then System.exit(exitCode)
