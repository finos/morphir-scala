package morphir.buildkit.internal

import kyo.*
import kyo.kernel.ArrowEffect

/**
 * Observe a raw non-fatal throw from `v`, whatever `v`'s effect row, and answer it with `handler`.
 *
 * Kyo 1.0.0-RC7 removed `Effect.catching`; a failure of a handled computation now reaches the `recover` arm of the
 * handler whose region it is in. `OnPanic` installs a region of its own for that purpose: the [[OnPanic.Guard]] effect
 * is never suspended, so the handler only ever answers through `recover`, and the row it adds is discharged before the
 * value leaves. This is how `kyo.Abort.catching` is built. Like the kernel's own recovery, it does not see a fatal
 * `Throwable`.
 */
private[buildkit] object OnPanic:

  sealed private trait Guard extends ArrowEffect[Const[Unit], Const[Unit]]

  def apply[A, S](v: => A < S)(handler: Throwable => A < S)(using Frame): A < S =
    ArrowEffect.handleCont(Tag[Guard], v: A < (Guard & S))(
      [C] => (_, cont) => cont(()),
      a => a,
      ex => Maybe(handler(ex))
    )
