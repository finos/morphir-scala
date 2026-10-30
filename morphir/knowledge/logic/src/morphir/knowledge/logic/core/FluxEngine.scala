package morphir.knowledge.logic.core

import kyo.*
import scala.collection.immutable.{Queue => ScalaQueue}

/**
 * Low-level round-robin (breadth-first) stream engine backing [[Flux]].
 *
 * A source is a pure, lazily unfolded sequence of chunks of `Option[A]`, where `None` is a yield-point "tick" used to
 * interleave fairly across concurrently-pending sources (mirrors the original `ZStream[Any, Nothing, Option[A]]`
 * representation).
 *
 * The engine pulls one chunk at a time and parks the remainder of each source in a queue. Through Kyo 1.0.0-RC6 a
 * source was a `kyo.Emit` computation pulled with `ArrowEffect.handleFirst`; RC7 made that primitive `private[kyo]` and
 * marks every public continuation `Region.NoEscape`, so a remainder can no longer be stored. The sources here own no
 * resources, so a plain lazy step type carries them without loss.
 */
private[core] object FluxEngine {

  /** One step of a source: done, or a chunk followed by the rest, which is not built until it is pulled. */
  enum Src[+A]:
    case Halt
    case Emitted(chunk: Chunk[Option[A]], rest: () => Src[A])

  private def emit[A](chunk: Chunk[Option[A]])(rest: => Src[A]): Src[A] = Src.Emitted(chunk, () => rest)

  private enum Step[+A]:
    case Halt
    case One(elem: Option[A], rest: Src[A])

  @annotation.tailrec
  private def pull1[A](src: Src[A]): Step[A] =
    src match
      case Src.Halt                 => Step.Halt
      case Src.Emitted(chunk, rest) =>
        if chunk.isEmpty then pull1(rest())
        else
          val tail = chunk.tail
          Step.One(chunk.head, if tail.isEmpty then rest() else Src.Emitted(tail, rest))

  // ---- Constructors ----
  def empty[A]: Src[A] = Src.Halt

  def succeed[A](a: A): Src[A] =
    emit(Chunk(Some(a): Option[A]))(Src.Halt)

  def repeat[A](a: A): Src[A] =
    def r: Src[A] =
      emit(Chunk(Some(a): Option[A]))(emit(Chunk(None: Option[A]))(r))
    r

  def suspend[A](src: => Src[A]): Src[A] =
    emit(Chunk(None: Option[A]))(src)

  // ---- The core BFS flatMap (faithful port of the ZIO ZStreamCompat state machine) ----
  private enum Item[+A, +B]:
    case Outer
    case Inner(src: Src[B])

  final private case class St[A, B](
      outerDone: Boolean,
      outer: Src[A],
      current: Option[Src[B]],
      queue: ScalaQueue[Item[A, B]]
  )

  def flatMap[A, B](src: Src[A])(f: A => Src[B]): Src[B] =
    def loop(st: St[A, B]): Src[B] =
      st.current match
        case Some(inner) =>
          pull1(inner) match
            case Step.Halt =>
              // inner exhausted: drop it, keep going
              loop(st.copy(current = None))
            case Step.One(Some(b), next) =>
              // emit the value, stay on this inner (consume its whole Some-run)
              emit(Chunk(Some(b): Option[B]))(loop(st.copy(current = Some(next))))
            case Step.One(None, next) =>
              // inner tick: NO output tick, just rotate this inner to the back
              loop(st.copy(current = None, queue = st.queue.enqueue(Item.Inner(next))))
        case None =>
          st.queue.headOption match
            case None =>
              Src.Halt // nothing left at all
            case Some(Item.Inner(innerSrc)) =>
              loop(st.copy(current = Some(innerSrc), queue = st.queue.tail))
            case Some(Item.Outer) =>
              if st.outerDone then
                if st.queue.size == 1 then Src.Halt
                else
                  // one output tick per full cycle, then rotate Outer to the back
                  emit(Chunk(None: Option[B]))(loop(st.copy(queue = st.queue.tail.enqueue(Item.Outer))))
              else
                pull1(st.outer) match
                  case Step.Halt =>
                    loop(st.copy(outerDone = true))
                  case Step.One(Some(a), nextOuter) =>
                    // spawn a new inner, make it current; Outer stays in the queue
                    loop(st.copy(outer = nextOuter, current = Some(f(a))))
                  case Step.One(None, nextOuter) =>
                    // outer tick: emit an output tick and rotate Outer to the back
                    emit(Chunk(None: Option[B]))(
                      loop(st.copy(outer = nextOuter, queue = st.queue.tail.enqueue(Item.Outer)))
                    )

    loop(St(outerDone = false, outer = src, current = None, queue = ScalaQueue(Item.Outer)))
  end flatMap

  def merge[A](left: Src[A], right: Src[A]): Src[A] =
    flatMap(emit(Chunk(Some(left): Option[Src[A]], Some(right): Option[Src[A]]))(Src.Halt))(identity)

  def runCollect[A](src: Src[A]): Chunk[A] =
    @annotation.tailrec
    def loop(s: Src[A], acc: Chunk[A]): Chunk[A] =
      pull1(s) match
        case Step.Halt               => acc
        case Step.One(None, next)    => loop(next, acc)
        case Step.One(Some(a), next) => loop(next, acc.append(a))
    loop(src, Chunk.empty)

  def runCollectN[A](src: Src[A], n: Long): Chunk[A] =
    @annotation.tailrec
    def loop(s: Src[A], acc: Chunk[A]): Chunk[A] =
      if acc.size >= n then acc
      else
        pull1(s) match
          case Step.Halt               => acc
          case Step.One(None, next)    => loop(next, acc)
          case Step.One(Some(a), next) => loop(next, acc.append(a))
    loop(src, Chunk.empty)
}
