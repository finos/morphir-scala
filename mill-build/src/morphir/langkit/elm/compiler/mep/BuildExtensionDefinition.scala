package morphir.langkit.elm.compiler.mep

/**
 * The release build's view of the provider's identity and capabilities.
 *
 * [[internal.ExtensionDefinition]] is `private[mep]`, so the published provider exposes none of it. The Mill meta-build
 * compiles that same source file, and this forwarder, which lives only in the meta-build and is never published, gives
 * the release build access to the one definition.
 */
object BuildExtensionDefinition:
  export internal.ExtensionDefinition.*
