package morphir.ui.services

import kyo.*

final case class WorkspaceRef(path: String) derives CanEqual, Schema

final case class PackageInfo(name: String, moduleCount: Int) derives CanEqual, Schema

final case class ModuleInfo(packageName: String, name: String, typeCount: Int, valueCount: Int)
    derives CanEqual, Schema

enum DefinitionKind derives CanEqual, Schema:
  case Type, Value

final case class DefinitionRef(packageName: String, moduleName: String, localName: String) derives CanEqual, Schema

final case class DefinitionDetail(ref: DefinitionRef, kind: DefinitionKind, summary: String) derives CanEqual, Schema

final case class BundleInfo(slug: String, title: String, conceptCount: Int) derives CanEqual, Schema

final case class ConceptRef(bundle: String, path: String) derives CanEqual, Schema

final case class ConceptDetail(ref: ConceptRef, conceptType: String, title: String, body: String)
    derives CanEqual, Schema

final case class IntentSummary(number: String, title: String, state: String, kind: String) derives CanEqual, Schema
