package morphir.ir.file.format

import morphir.ir.module.ModuleName

case class MirFile (header:MirFileHeader)
case class MirFileHeader(formatVersion:MirFileFormatVersion, moduleName:ModuleName)
case class MirFileFormatVersion(major:Int, minor:Int, revision:Int)
