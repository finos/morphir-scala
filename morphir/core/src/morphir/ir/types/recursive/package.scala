package morphir.ir.types

package object recursive {

  type Result[+E, +A] = Either[E, A]
  val Result: Either.type = Either

  type UType = morphir.ir.types.recursive.Type.UType
  val UType: morphir.ir.types.recursive.Type.type = morphir.ir.types.recursive.Type.UType
}
