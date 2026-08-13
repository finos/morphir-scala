package morphir.buildkit

import morphir.MorphirException

/**
 * A stage hid an `Abort` inside its declared `S` effect row instead of its declared `E` — something
 * [[morphir.buildkit.Stage.apply]]'s own scaladoc warns cannot be forbidden statically (Scala has no negative evidence
 * for "this row does not carry `Abort[X]`"), since a caller can always pick `E := Nothing` and fold the real error into
 * `S` instead. The executor's own choke points ([[SealedPipeline#execute]], [[SealedPipeline#runReport]]) catch that at
 * runtime instead: every `Abort` failure that reaches them, declared or not, is observed — Kyo dispatches every
 * `Abort[X]` through one erased tag, so a handler summoned at `ConcreteTag[Any]` is offered a hidden failure exactly as
 * it would the declared one — and one whose value the pipeline's own declared `E` does not accept is converted to this
 * panic rather than left to escape silently through `S`, unbalancing the event stream (a `NodeStarted` with no matching
 * `NodeFinished`/`RunFinished`) the way it used to before `Stage.fromKyo` was removed.
 *
 * `error` is the original value the hidden `Abort` was raised with, preserved here (not just in the message string) for
 * a caller that wants to inspect it programmatically — [[NodeOutcome.Failed]] carries the whole [[kyo.Result.Panic]],
 * so this exception is reachable from a report the same way any other panic is.
 *
 * The core message is always present. Detail from `error` is appended when rendering succeeds; when `String.valueOf` /
 * `toString` throws, that failure note is appended instead — never substituting away the core — because
 * `executeIntercepted` builds this exception while assembling the cleanup continuation, before `closeOpenNodes` /
 * `RunFinished(false)` have run, and a throw there would leave the event stream unbalanced again.
 */
final class UndeclaredAbortException(val error: Any)
    extends MorphirException(UndeclaredAbortException.message(error))

object UndeclaredAbortException:
  private val coreMessage =
    "a stage hid Abort(...) inside its declared effect row instead of its declared error channel"

  private def message(error: Any): String =
    try s"$coreMessage: ${String.valueOf(error)}"
    catch
      case t: Throwable =>
        s"$coreMessage: <error toString failed: ${t.getClass.getName}>"
