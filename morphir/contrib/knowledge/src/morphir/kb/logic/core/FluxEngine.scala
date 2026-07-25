package morphir.knowledge.logic.core

import kyo.*
import kyo.kernel.*
import scala.collection.immutable.{Queue => ScalaQueue}

/**
 * Low-level round-robin (breadth-first) stream engine backing [[Flux]], built on the kyo-prelude `Emit` effect and the
 * kyo-kernel `ArrowEffect.handleFirst` pull primitive.
 *
 * A source is a pure Emit computation of chunks of `Option[A]`, where `None` is a yield-point "tick" used to interleave
 * fairly across concurrently-pending sources (mirrors the original `ZStream[Any, Nothing, Option[A]]` representation).
 */
private[core] object FluxEngine {

  type Src[A] = Unit < Emit[Chunk[Option[A]]]

  // ---- Pull primitive: peel exactly ONE emitted chunk, WITHOUT resuming ----
  // (the analogue of ZStream#toPull)
  private enum Pulled[+A]:
    case Halt
    case Emitted(chunk: Chunk[Option[A]], rest: Src[A])

  private def pullChunk[A](src: Src[A])(using tag: Tag[Emit[Chunk[Option[A]]]], frame: Frame): Pulled[A] =
    ArrowEffect.handleFirst(tag, src)(
      handle = [C] => (chunk: Chunk[Option[A]], cont) => (Pulled.Emitted(chunk, cont(())): Pulled[A]),
      done = (_: Unit) => (Pulled.Halt: Pulled[A])
    ).eval

  private enum Step[+A]:
    case Halt
    case One(elem: Option[A], rest: Src[A])

  private def pull1[A](src: Src[A])(using Tag[Emit[Chunk[Option[A]]]], Frame): Step[A] =
    pullChunk(src) match
      case Pulled.Halt                 => Step.Halt
      case Pulled.Emitted(chunk, rest) =>
        if chunk.isEmpty then pull1(rest)
        else
          val head          = chunk.head
          val tail          = chunk.tail
          val rest2: Src[A] = if tail.isEmpty then rest else Emit.value(tail).andThen(rest)
          Step.One(head, rest2)

  // ---- Constructors ----
  def empty[A](using Tag[Emit[Chunk[Option[A]]]], Frame): Src[A] = ()

  def succeed[A](a: A)(using Tag[Emit[Chunk[Option[A]]]], Frame): Src[A] =
    Emit.value(Chunk(Some(a): Option[A]))

  def repeat[A](a: A)(using Tag[Emit[Chunk[Option[A]]]], Frame): Src[A] =
    def r: Src[A] =
      Emit.value(Chunk(Some(a): Option[A]))
        .andThen(Emit.value(Chunk(None: Option[A])))
        .andThen(r)
    r

  def suspend[A](src: => Src[A])(using Tag[Emit[Chunk[Option[A]]]], Frame): Src[A] =
    Emit.value(Chunk(None: Option[A])).andThen(src)

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

  def flatMap[A, B](src: Src[A])(f: A => Src[B])(using
      ta: Tag[Emit[Chunk[Option[A]]]],
      tb: Tag[Emit[Chunk[Option[B]]]],
      frame: Frame
  ): Src[B] =
    def loop(st: St[A, B]): Src[B] =
      st.current match
        case Some(inner) =>
          pull1(inner) match
            case Step.Halt =>
              // inner exhausted: drop it, keep going
              loop(st.copy(current = None))
            case Step.One(Some(b), next) =>
              // emit the value, stay on this inner (consume its whole Some-run)
              Emit.value(Chunk(Some(b): Option[B])).andThen(loop(st.copy(current = Some(next))))
            case Step.One(None, next) =>
              // inner tick: NO output tick, just rotate this inner to the back
              loop(st.copy(current = None, queue = st.queue.enqueue(Item.Inner(next))))
        case None =>
          st.queue.headOption match
            case None =>
              () // nothing left at all
            case Some(Item.Inner(innerSrc)) =>
              loop(st.copy(current = Some(innerSrc), queue = st.queue.tail))
            case Some(Item.Outer) =>
              if st.outerDone then
                if st.queue.size == 1 then ()
                else
                  // one output tick per full cycle, then rotate Outer to the back
                  Emit.value(Chunk(None: Option[B]))
                    .andThen(loop(st.copy(queue = st.queue.tail.enqueue(Item.Outer))))
              else
                pull1(st.outer) match
                  case Step.Halt =>
                    loop(st.copy(outerDone = true))
                  case Step.One(Some(a), nextOuter) =>
                    // spawn a new inner, make it current; Outer stays in the queue
                    loop(st.copy(outer = nextOuter, current = Some(f(a))))
                  case Step.One(None, nextOuter) =>
                    // outer tick: emit an output tick and rotate Outer to the back
                    Emit.value(Chunk(None: Option[B]))
                      .andThen(loop(st.copy(outer = nextOuter, queue = st.queue.tail.enqueue(Item.Outer))))

    loop(St(outerDone = false, outer = src, current = None, queue = ScalaQueue(Item.Outer)))
  end flatMap

  // A plain (non-Kyo) wrapper around a source. Emit requires a Tag for its element
  // type; the Kyo pending-computation type `Src[A]` is not Tag-derivable (the Tag
  // macro hits an HKTypeLambda MatchError on `<`/`Kyo`), so we box it in an ordinary
  // case class whose Tag needs only `Tag[A]`.
  final private case class Boxed[A](src: Src[A])

  def merge[A: Tag](left: Src[A], right: Src[A])(using frame: Frame): Src[A] =
    val outer: Src[Boxed[A]] =
      Emit.value(Chunk(Some(Boxed(left)): Option[Boxed[A]], Some(Boxed(right)): Option[Boxed[A]]))
    flatMap[Boxed[A], A](outer)(_.src)

  def runCollect[A](src: Src[A])(using Tag[Emit[Chunk[Option[A]]]], Frame): Chunk[A] =
    def go(s: Src[A], acc: Chunk[A]): Chunk[A] =
      pull1(s) match
        case Step.Halt               => acc
        case Step.One(None, next)    => go(next, acc)
        case Step.One(Some(a), next) => go(next, acc.append(a))
    go(src, Chunk.empty)

  def runCollectN[A](src: Src[A], n: Long)(using Tag[Emit[Chunk[Option[A]]]], Frame): Chunk[A] =
    def go(s: Src[A], acc: Chunk[A]): Chunk[A] =
      if acc.size >= n then acc
      else
        pull1(s) match
          case Step.Halt               => acc
          case Step.One(None, next)    => go(next, acc)
          case Step.One(Some(a), next) => go(next, acc.append(a))
    go(src, Chunk.empty)
}
