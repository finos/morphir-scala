package morphir.langkit.elm.compiler.mep

object Main:
  private[mep] val providerMetadata: ProviderMetadata = providerMetadata(
    MepBuildInfo.providerId,
    MepBuildInfo.providerName,
    MepBuildInfo.providerVersion
  )

  private[mep] def providerMetadata(id: String, name: String, version: String): ProviderMetadata =
    ProviderMetadata.default.copy(id = id, name = name, version = version)

  def main(args: Array[String]): Unit =
    val exitCode = MepProcess.run(System.in, System.out, System.err, providerMetadata)
    if exitCode != 0 then System.exit(exitCode)
