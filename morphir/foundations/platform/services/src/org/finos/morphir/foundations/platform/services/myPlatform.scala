package org.finos.morphir.foundations.platform.services
import org.finos.morphir.foundations.platform.services.internal.{FsModule, PathApi, PathModule}

object myPlatform {
  final type Path = FsModule.Path

  def isWindows(): Boolean = scala.util.Properties.isWin
  object fs {
    object exists extends Function1[Path, Boolean] {
      def apply(path: Path): Boolean = FsModule.exists(path)
    }
    
    def root: Path = FsModule.root
  }
  object path extends PathApi {
    def delimiter: String = PathModule.delimiter
    def sep: String       = PathModule.sep
  }
  object process {}
}
