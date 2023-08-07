package org.finos.morphir.extensibility
import org.finos.morphir.MorphirTag
import org.finos.morphir.naming.*

sealed trait ModuleDescriptor { self =>
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

sealed abstract class SdkModuleDescriptor(moduleName: String)(implicit packageName: PackageName)
    extends ModuleDescriptor {
  override implicit val qualifiedModuleName: QualifiedModuleName =
    QualifiedModuleName(packageName, ModuleName.fromString(moduleName))
}

object SdkModuleDescriptors {
  object Morphir {
    object SDK {
      implicit val packageName: PackageName = PackageName.fromString("Morphir.SDK")

      // TODO: Write a test to check what MorphirTag looks like for all of these

      // NOTE: We will want to use the MorphirTag to look up info of Modules, Types, and Valuesi      
      case object Basics extends SdkModuleDescriptor("Basics") {
        // type Type = Basics.type

        // TODO: Provide Function Descriptors
      }

      case object String extends SdkModuleDescriptor("String") {
        // type Type = String.type
      }

      case object List extends SdkModuleDescriptor("List") {
        // type Type = List.type
      }

      case object Maybe extends SdkModuleDescriptor("Maybe") {
        // type Type = Maybe.type
      }
    }
  }
}
