package millbuild.millmorphir.elm
import mill._
import mill.scalalib._
import millbuild.millmorphir.MorphirModule

trait MorphirElmModule extends MorphirModule {
    def allSourceFiles: T[Seq[PathRef]] = T {
        sources().map(_.path).flatMap(os.walk(_).filter(_.ext == "elm")).map(PathRef(_))
    }
}