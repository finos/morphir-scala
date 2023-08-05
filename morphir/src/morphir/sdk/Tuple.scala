package morphir.sdk

import org.finos.morphir.extensibility.*

object Tuple extends SdkModule("Morphir.Sdk", "Tuple") {
  // TODO: Requires PolyFunction
  // object first extends NativeFunction1[(Tuple2[A, B]), A] {
  //   override def description: String           = "Returns the first element of a tuple."
  //   override def apply(tuple: Tuple2[A, B]): A = tuple._1
  // }
}
