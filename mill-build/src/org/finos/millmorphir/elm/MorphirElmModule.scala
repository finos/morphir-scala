package org.finos.millmorphir.elm
import mill.*
import mill.scalalib.*
import org.finos.millmorphir.MorphirModule

trait MorphirElmModule extends MorphirModule {
  def allSourceFiles: T[Seq[PathRef]] = Task {
    sources().map(_.path).flatMap(os.walk(_).filter(_.ext == "elm")).map(PathRef(_))
  }

  def morphirProjectSourceFileNames = Task {
    super.morphirProjectSourceFileNames() ++ Set("elm.json")
  }
}
