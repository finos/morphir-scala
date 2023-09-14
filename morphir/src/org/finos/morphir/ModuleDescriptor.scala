package org.finos.morphir

import org.finos.morphir.naming._
import org.finos.morphir.MorphirTag

trait ModuleDescriptor { self =>
  type Type = self.type

  final def getTag: MorphirTag[Type] = morphirTag

  implicit lazy val nodeID: NodeID = NodeID.ModuleID.fromQualifiedName(qualifiedModuleName)
  implicit def qualifiedModuleName: QualifiedModuleName

  final implicit def morphirTag: MorphirTag[Type] = new MorphirTag[Type] {
    override def nodeID: NodeID = NodeID.fromQualifiedName(qualifiedModuleName)
  }

  final implicit def hasShapeTag: MorphirTag.Has[Type] = new MorphirTag.Has[Type] {
    override def getTag: MorphirTag[Type] = self.getTag
  }
}

object ModuleDescriptor {
  type For[A] = ModuleDescriptor { type Type = A }
}
