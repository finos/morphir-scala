package org.finos.millmorphir.elm
import mill._
import mill.scalalib._
import org.finos.millmorphir.MorphirModule

trait MorphirElmModule extends MorphirModule {
  def allSourceFiles: T[Seq[PathRef]] = T {
    sources().map(_.path).flatMap(os.walk(_).filter(_.ext == "elm")).map(PathRef(_))
  }

  def morphirProjectSourceFileNames = T {
    super.morphirProjectSourceFileNames() ++ Set("elm.json")
  }
}
