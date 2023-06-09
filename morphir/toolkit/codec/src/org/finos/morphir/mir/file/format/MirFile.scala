package org.finos
package morphir
package mir
package file.format

import org.finos.morphir.ir.Module.QualifiedModuleName

final case class MirFile(header: MirFileHeader)
object MirFile {

  def apply(moduleName: QualifiedModuleName): MirFile =
    MirFile(MirFileHeader(formatVersion = MirFileFormatVersion.latest, moduleName))
}
final case class MirFileHeader(formatVersion: MirFileFormatVersion, moduleName: QualifiedModuleName)
final case class MirFileFormatVersion(major: Int, minor: Int, revision: Int)
object MirFileFormatVersion {
  val latest: MirFileFormatVersion = MirFileFormatVersion(0, 1, 0)
}
