package morphir.langkit.elm.compiler.mep.internal

/** Compiled by both the provider and the release build; no runtime-specific JSON dependency. */
private[mep] object ExtensionDefinition:
  val Id                       = "morphir-scala-elm"
  val ShortId                  = "scala-elm"
  val Name                     = "Morphir Scala Elm frontend"
  val DefaultVersion           = "0.1.0"
  val ClaimsVersion            = "0.1.0-draft.2"
  val ProtocolVersion          = "0.1"
  val WorkspaceProtocolVersion = "0.1.0-draft.1"
  val Types                    = Seq("frontend", "workspace")

  // JSON preserves the capability member order across Kyo (the provider) and ujson (the build).
  val CapabilitiesJson: String = s"""{
    "frontend": {
      "languages": [{"id": "elm", "fileExtensions": [".elm"]}],
      "irVersions": ["3"],
      "compile": true,
      "incremental": false,
      "fragments": false,
      "multiDocument": false
    },
    "workspace": {"protocolVersions": ["$WorkspaceProtocolVersion"], "discover": true},
    "streaming": false,
    "incremental": false,
    "cancellation": false,
    "progress": false
  }"""
