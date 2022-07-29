package morphir

package object runtime {
  @noinline def throwUndefined(): Nothing =
    throw new UndefinedBehaviorError

  def intrinsic: Nothing = throwUndefined()
}
