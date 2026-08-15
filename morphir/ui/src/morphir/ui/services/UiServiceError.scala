package morphir.ui.services

import morphir.MorphirException

enum UiServiceError(message: String) extends MorphirException(message):
  case WorkspaceNotFound(path: String)               extends UiServiceError(s"workspace not found: $path")
  case ConceptNotFound(bundle: String, path: String) extends UiServiceError(s"concept not found: $bundle:$path")
  case DefinitionNotFound(ref: DefinitionRef)
      extends UiServiceError(s"definition not found: ${ref.packageName}:${ref.moduleName}:${ref.localName}")
