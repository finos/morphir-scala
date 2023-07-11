package org.finos.morphir.foundations.platform.services.internal

trait FsApi {
  type Path
  def root:Path 
  def exists(path: Path): Boolean  
}
