package morphir.mir.types

package object recursive {

  type Result[+E, +A] = Either[E, A]
  val Result: Either.type = Either

  type UType = morphir.mir.types.recursive.Type.UType
  val UType: morphir.mir.types.recursive.Type.type = morphir.mir.types.recursive.Type.UType
}
