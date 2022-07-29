package morphir.mir.file.format

import morphir.mir.module.ModuleName

final case class MirFile(header: MirFileHeader)
object MirFile {

  def apply(moduleName: ModuleName): MirFile =
    MirFile(MirFileHeader(formatVersion = MirFileFormatVersion.latest, moduleName))
}
final case class MirFileHeader(formatVersion: MirFileFormatVersion, moduleName: ModuleName)
final case class MirFileFormatVersion(major: Int, minor: Int, revision: Int)
object MirFileFormatVersion {
  val latest: MirFileFormatVersion = MirFileFormatVersion(0, 1, 0)
}
