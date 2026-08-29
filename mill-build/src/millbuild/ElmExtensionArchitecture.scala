package millbuild

object ElmExtensionArchitecture:
  final case class Source(path: String, text: String)

  private val ForbiddenSourceTokens = Seq(
    "org.finos.morphir.ir",
    "dev.zio",
    "zio.json"
  )

  def violations(
      sources: Seq[Source],
      compilerManifest: String,
      compatibilityManifest: String,
      mepManifest: String
  ): Seq[String] =
    val sourceViolations = sources.flatMap { source =>
      ForbiddenSourceTokens.collect {
        case token if source.text.contains(token) =>
          s"${source.path} contains $token"
      }
    }

    sourceViolations ++
      requireContains("compiler manifest", compilerManifest, "build.morphir.model.jvm") ++
      requireExcludes("compiler manifest", compilerManifest, "build.morphir.jvm") ++
      requireContains("v3 compatibility manifest", compatibilityManifest, "build.morphir.model.jvm") ++
      requireExcludes("v3 compatibility manifest", compatibilityManifest, "build.morphir.jvm") ++
      requireExcludes("v3 compatibility manifest", compatibilityManifest, "build.morphir.interop.zio") ++
      requireExcludes("v3 compatibility manifest", compatibilityManifest, "dev.zio") ++
      requireContains("MEP manifest", mepManifest, "build.MorphirKyoSchemaJsonMvnDeps") ++
      requireContains("MEP manifest", mepManifest, "io.getkyo::kyo-jsonrpc") ++
      requireContains("MEP manifest", mepManifest, "build.morphir.model.compat.v3.jvm") ++
      requireExcludes("MEP manifest", mepManifest, "build.morphir.interop.zio") ++
      requireExcludes("MEP manifest", mepManifest, "dev.zio")

  private def requireContains(label: String, text: String, expected: String): Seq[String] =
    if text.contains(expected) then Seq.empty else Seq(s"$label must contain $expected")

  private def requireExcludes(label: String, text: String, forbidden: String): Seq[String] =
    if text.contains(forbidden) then Seq(s"$label must not contain $forbidden") else Seq.empty
