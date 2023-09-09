package org.finos.morphir.util.vfile
import java.nio.file.Path

trait VFilePlatformSpecific {
  def fileRef(path: Path): VFile = VFile.fileRef(VPath(path))
}
