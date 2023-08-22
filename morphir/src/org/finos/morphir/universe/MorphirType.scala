package org.finos.morphir.universe

import org.finos.morphir.naming.*
import org.finos.morphir.universe.ir.*

//TODO: Make a sealed trait for IntrinisicTypeSpec and IntrinsicTypeDef (which is private only to the morphir library), TypeSpec and TypeDef, and ExtermalTypeSpec
sealed trait MorphirType extends HasConceptId {}

object MorphirType {
  final case class IntrinsicTypeSpec(id: ConceptId, underlying: TypeSpecification[Attributes], fqName: FQName)
      extends MorphirType
  final case class IntrinsicTypeDef(id: ConceptId, underlying: TypeDefinition[Attributes])    extends MorphirType
  final case class TypeSpec(id: ConceptId, underlying: TypeSpecification[Attributes])         extends MorphirType
  final case class TypeDef(id: ConceptId, underlying: TypeDefinition[Attributes])             extends MorphirType
  final case class ExternalTypeSpec(id: ConceptId, underlying: TypeSpecification[Attributes]) extends MorphirType
}
