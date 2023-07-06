package org.finos.morphir.util
import ZResult.*

sealed trait ZResult[+W,+E,+A] { self =>  

  /**
   * A symbolic alias for `zipParLeft`.
   */
  final def <&[W1 >: W, E1 >: E, B](that: => ZResult[W1, E1, B]): ZResult[W1, E1, A] = zipParLeft(that)

  /**
   * A symbolic alias for `zipParRight`.
   */
  final def &>[W1 >: W, E1 >: E, B](that: => ZResult[W1, E1, B]): ZResult[W1, E1, B] = zipParRight(that)

  /**
   * A symbolic alias for `zipPar`.
   */
  final def <&>[W1 >: W, E1 >: E, B](that: => ZResult[W1, E1, B]): ZResult[W1, E1, (A, B)] = zipPar(that)

  /**
   * A symbolic alias for `log`.        
   */
  final def ??[W1 >: W](entry: W1): ZResult[W1,E,A] = log(entry)

  /**
   * Maps the successful value of this `ZResult` to the specified constant value.
   */
  final def as[B](b: => B): ZResult[W,E,B] = map(_ => b)

  /**
   * Maps the error value of this `ZResult` to the specified constant value.
   */
  final def asError[E2](e: => E2): ZResult[W,E2,A] = mapError(_ => e)

  /**
   * Returns the value, because no eror has occurred.
   */
  final def get(implicit ev: E <:< Nothing):A = self.asInstanceOf[Success[W,A]].value

  /**
   * Returns the value, if successful, otherwise returns the `fallback` value.
   */
  final def getOrElse[A1 >: A](fallback: => A1): A1 = self match {
    case Failure(_, _) => fallback
    case Success(_, a) => a
  }

  /**
   * Transforms the value of this `ZResult` with the specified result returning 
   * function if it is a success or returns the value unchanged otherwise.
   */
  def flatMap[W1 >: W, E1 >: E, B](f: A => ZResult[W1, E1, B]):ZResult[W1, E1, B] = self match {
    case Failure(log, errors) => Failure(log, errors)
    case Success(log, value)  => 
      f(value) match {
        case Failure(logR, errors) => Failure(log ++ logR, errors)
        case Success(logR, value) => Success(log ++ logR, value)
      }
  }

  /**
   * Flattens nested `ZResult`s.   
   */
  final def flatten[W1 >: W, E1 >: E, B](implicit ev: A <:< ZResult[W1, E1, B]): ZResult[W1, E1, B] = self.flatMap(a => ev(a))

  /**
   * Folds over the error and success values of this `ZResult`.
   */
  final def fold[B](failure: NonEmptyChunk[E] => B, success: A => B): B = self match {
    case Failure(_, errors) => failure(errors)
    case Success(_, value)  => success(value)
  }

  /**
   * Writes an entry to the log.
   */
  final def log[W1 >: W](entry: W1): ZResult[W1,E,A] = self match {
    case Failure(log, errors) => Failure(log :+ entry, errors)
    case Success(log, value)  => Success(log :+ entry, value)
  }

  /**
   * Transforms the successful value of this `ZResult` with the specified function `f`.
   */
  final def map[B](f: A => B): ZResult[W,E,B] = self match {
    case Failure(log, errors) => Failure(log, errors)
    case Success(log, value)  => Success(log, f(value))
  }

  /**
   * Transforms the error value of this `ZResult` with the specified function `f`.
   */
  final def mapError[E1](f: E => E1): ZResult[W,E1,A] = self match {
    case Failure(log, errors) => Failure(log, errors.map(f))
    case Success(log, value)  => Success(log, value)
  }

  /**
   * Transforms the log entries of this `ZResult` with the specified function `f`.   
   */
  final def mapLog[W2](f: W => W2): ZResult[W2,E,A] = self match {
    case Failure(log, errors) => Failure(log.map(f), errors)
    case Success(log, value)  => Success(log.map(f), value)
  }

  /**
   * Transforms all the log entries of this `ZResult` with the specified function `f`.   
   */
  final def mapLogAll[W2](f: Chunk[W] => Chunk[W2]): ZResult[W2,E,A] = self match {
    case Failure(log, errors) => Failure(f(log), errors)
    case Success(log, value)  => Success(f(log), value)
  }

  def runLog[B]:(Chunk[W], Either[NonEmptyChunk[E], A]) = self match {
    case Failure(log, errors) => (log, Left(errors))
    case Success(log, value)  => (log, Right(value))
  }

  final def orElse[W1 >: W, E1, A1 >: A](that: ZResult[W1, E1, A1]): ZResult[W1, E1, A1] = self match {
    case Failure(log, errors) => that.mapLogAll(log ++ _)
    case Success(log, value) => Success(log, value)
  }

  final def orElseLog[W1 >: W, E1, A1 >: A](
    that: ZResult[W1, E1, A1]
  )(implicit ev: E <:< W1): ZResult[W1, E1, A1] =
    self match {
      case Failure(log, errors) => that.mapLogAll(log ++ errors.map(ev) ++ _)
      case Success(log, value)  => Success(log, value)
    }

  /**
   * Transforms this `ZResult` into an `Either`, discarding the log.
   */
  final def toEither: Either[NonEmptyChunk[E], A] = fold(Left(_), Right(_))

  /**
   * Transforms this `ZResult` into an `Option`, discarding information about
   * the errors and the log.
   */
  final def toOption: Option[A] = fold(_ => None, Some(_))

  /**
   * Transforms this `ZResult` into a `Try`, discarding all but the first error and the log.
   */
  final def toTry(implicit ev: E <:< Throwable): scala.util.Try[A] = fold(es => scala.util.Failure(es.head), scala.util.Success(_))


  /**
   * Combines this `ZResult` with the specified `ZResult`, returning a
   * tuple of their results. Returns either the combined result if both were
   * successes or otherwise returns a failure with all errors.
   */
  final def zipPar[W1 >: W, E1 >: E, B](that: ZResult[W1, E1, B]): ZResult[W1, E1, (A, B)] =
    zipWithPar(that)((_, _))

  /**
   * A variant of `zipPar` that keeps only the left success value, but returns
   * a failure with all errors if either this `ZResult` or the specified
   * `ZResult` fail.
   */
  final def zipParLeft[W1 >: W, E1 >: E, B](that: ZResult[W1, E1, B]): ZResult[W1, E1, A] =
    zipWithPar(that)((a, _) => a)

  /**
   * A variant of `zipPar` that keeps only the right success value, but returns
   * a failure with all errors if either this `ZResult` or the specified
   * `ZResult` fail.
   */
  final def zipParRight[W1 >: W, E1 >: E, B](that: ZResult[W1, E1, B]): ZResult[W1, E1, B] =
    zipWithPar(that)((_, b) => b)

  /**
   * Combines this `ZResult` with the specified `ZResult`, using the
   * function `f` to combine their success values. Returns either the combined
   * result if both were successes or otherwise returns a failure with all
   * errors.
   */
  final def zipWithPar[W1 >: W, E1 >: E, B, C](that: ZResult[W1, E1, B])(f: (A, B) => C): ZResult[W1, E1, C] =
    (self, that) match {
      case (Failure(w, e), Failure(w1, e1)) => Failure(w ++ w1, e ++ e1)
      case (Failure(w, e), Success(w1, _))  => Failure(w ++ w1, e)
      case (Success(w, _), Failure(w1, e1)) => Failure(w ++ w1, e1)
      case (Success(w, a), Success(w1, b))  => Success(w ++ w1, f(a, b))
    }
}

object ZResult {
  final case class Failure[+W,+E](log:Chunk[W], errors:NonEmptyChunk[E]) extends ZResult[W,E,Nothing]
  final case class Success[+W,+A](log:Chunk[W], value:A) extends ZResult[W,Nothing,A]  

  /**
   * Attempts to evaluate the specified value, catching any error that occurs
   * during evaluation and capturing it as a failure.
   */
  def apply[A](a: => A): Result[Throwable, A] =
    try succeed(a)
    catch {
      case e: VirtualMachineError => throw e
      case e: Throwable           => fail(e)
    }

  /**
   * Constructs a `ZResult` that fails with the specified error.
   */
  def fail[E](error: E): Result[E, Nothing] =
    Failure(Chunk.empty, NonEmptyChunk(error))

  /**
   * Constructs a `ZResult` that fails with the specified `NonEmptyChunk`
   * of errors.
   */
  def failNonEmptyChunk[E](errors: NonEmptyChunk[E]): Result[E, Nothing] =
    Failure(Chunk.empty, errors)    

  /**
   * Constructs a `ZResult` from an `Either`.
   */
  def fromEither[E, A](value: Either[E, A]): Result[E, A] =
    value.fold(fail, succeed)

  /**
   * Constructs a `ZResult` from an `Either` that fails with a
   * `NonEmptyChunk` of errors.
   */
  def fromEitherNonEmptyChunk[E, A](value: Either[NonEmptyChunk[E], A]): Result[E, A] =
    value.fold(failNonEmptyChunk, succeed)

  /**
   * Constructs a `ZResult` from an `Option`.
   */
  def fromOption[A](value:Option[A]): Result[Unit, A] =
    value.fold[Result[Unit, A]](fail(()))(succeed)

  /**
   * Constructs a `Result` from an `Option`, failing with the error
   * provided.
   */
  def fromOptionWith[E,A](error: => E)(value:Option[A]): Result[E, A] =
    value.fold[Result[E, A]](fail(error))(succeed)

  /**
   * Constructs a `Result` from a predicate, failing with None.
   */
  def fromPredicate[A](value: A)(f: A => Boolean): Result[None.type, A] =
    fromPredicateWith(None)(value)(f)

  /**
   * Constructs a `Result` from a predicate, failing with the error provided.
   */
  def fromPredicateWith[E, A](error: => E)(value: A)(f: A => Boolean): Result[E, A] =
    if (f(value)) Result.succeed(value)
    else Result.fail(error)    

  /**
   * Constructs a `ZResult` from a `Try`.
   */
  def fromTry[A](value: => scala.util.Try[A]): Result[Throwable, A] =
    value.fold(fail, succeed)

  /**
   * Constructs a `ZResult` that succeeds with the `Unit` value with a log
   * containing the specified entry.
   */
  def log[W](entry: W): ZResult[W, Nothing, Unit] =
    Success(Chunk(entry), ())    

  /**
   * Constructs a `Result` that succeeds with the specified value.
   */
  def succeed[A](value: A): Result[Nothing, A] =
    Success(Chunk.empty, value)

  /**
   * The `Result` that succeeds with the `Unit` value.
   */
  val unit: Result[Nothing, Unit] =
    succeed(())
}
