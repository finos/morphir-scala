package morphir.ui.services

import kyo.Schema
import morphir.MorphirException

enum UiServiceError(message: String) extends MorphirException(message) derives Schema:
  case WorkspaceNotFound(path: String)               extends UiServiceError(s"workspace not found: $path")
  case ConceptNotFound(bundle: String, path: String) extends UiServiceError(s"concept not found: $bundle:$path")
  case DefinitionNotFound(ref: DefinitionRef)
      extends UiServiceError(s"definition not found: ${ref.packageName}:${ref.moduleName}:${ref.localName}")

object UiServiceError:
  /** Wire code for morphir-ui domain errors, in the JSON-RPC server-defined range. */
  val wireCode: Int       = -32001
  val wireMessage: String = "morphir service error"
