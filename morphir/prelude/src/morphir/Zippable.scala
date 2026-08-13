package morphir

/**
 * Tuple-flattening pairing, after ZIO's `Zippable`: zipping a tuple with a value extends the tuple rather than nesting
 * — `(A, B)` zip `C` is `(A, B, C)` — so chained `par` calls read as one flat product. Implemented with Scala 3 generic
 * tuples rather than enumerated arity instances. `Unit`-discarding variants are deliberately omitted until a consumer
 * needs them.
 *
 * Declared in package `morphir` (kyo-style flat namespace), shipped from the buildkit core artifact; nothing about
 * pairing is buildkit-specific. Bead morphir-lt3 tracks extracting the shared `morphir` types into a prelude module.
 *
 * '''On the `=:=` evidence parameter.''' Each tuple-producing given below carries an `O` type parameter proven equal to
 * the `Tuple.Concat` result via a `using ev: O =:= Tuple.Concat[...]` clause, rather than writing
 * `type Out = Tuple.Concat[...]` directly. This is not stylistic: `given bothTuples[A <: Tuple, B <: Tuple]: Aux[A, B,
 * Tuple.Concat[A, B]]` compiles, and resolves `summon[Zippable[(Int, String), (Boolean, Long)]]` correctly, but fails
 * `summon[Zippable.Aux[(Int, String), (Boolean, Long), (Int, String, Boolean, Long)]]` — the search cannot verify the
 * match-type-valued `Out` member against the caller's concrete `O` while `A`/`B` are still being inferred, even though
 * `Tuple.Concat[(Int, String), (Boolean, Long)] =:= (Int, String, Boolean, Long)` holds and checks fine as a standalone
 * `summon`. Routing the check through `O` and a separate `=:=` search sidesteps it: `O` unifies trivially from the
 * `Aux` arguments, and the `=:=` requirement is then solved on its own, where match types do reduce. Confirmed with
 * `scala-cli` against Scala 3.8.4 outside the Mill build before landing here.
 */
trait Zippable[-A, -B]:
  type Out
  def zip(a: A, b: B): Out

object Zippable extends ZippableLowPriority1:
  type Aux[A, B, O] = Zippable[A, B] { type Out = O }

  given bothTuples[A <: Tuple, B <: Tuple, O](using ev: O =:= Tuple.Concat[A, B]): Aux[A, B, O] =
    new Zippable[A, B]:
      type Out = O
      def zip(a: A, b: B): Out = ev.flip(a ++ b)

private[morphir] trait ZippableLowPriority1 extends ZippableLowPriority2:
  given leftTuple[A <: Tuple, B, O](using ev: O =:= Tuple.Concat[A, B *: EmptyTuple]): Zippable.Aux[A, B, O] =
    new Zippable[A, B]:
      type Out = O
      def zip(a: A, b: B): Out = ev.flip(a ++ (b *: EmptyTuple))

private[morphir] trait ZippableLowPriority2 extends ZippableLowPriority3:
  given rightTuple[A, B <: Tuple]: Zippable.Aux[A, B, A *: B] =
    new Zippable[A, B]:
      type Out = A *: B
      def zip(a: A, b: B): Out = a *: b

private[morphir] trait ZippableLowPriority3:
  given pair[A, B]: Zippable.Aux[A, B, (A, B)] =
    new Zippable[A, B]:
      type Out = (A, B)
      def zip(a: A, b: B): Out = (a, b)
