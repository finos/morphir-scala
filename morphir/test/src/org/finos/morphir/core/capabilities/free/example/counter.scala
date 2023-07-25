package org.finos.morphir.core.capabilities.free.example
import org.finos.morphir.core.capabilities.free.*

object counter {
  object grammar {
    sealed trait CounterExpr[+E, +A]
    object CounterExpr {
      final case class Increment(n: Int)     extends CounterExpr[Nothing, Unit]
      final case class Decrement(n: Int)     extends CounterExpr[Nothing, Unit]
      final case class Get()                 extends CounterExpr[Nothing, Int]
      final case class Overflow(target: Int) extends CounterExpr[CounterError.Overflow, Nothing]
    }

    sealed trait IncrementOrDecrement
    object IncrementOrDecrement {
      case object Increment extends IncrementOrDecrement
      case object Decrement extends IncrementOrDecrement
    }

    sealed abstract class CounterError extends RuntimeException with Product with Serializable
    object CounterError {
      final case class Overflow(current: Int, target: Int) extends CounterError
    }
  }
  object dsl {
    import grammar.*
    // Create a Free dsl over the counter grammar
    type Counter[+E, +A] = Free[CounterExpr, E, A]

    def increment(n: Int): Counter[Nothing, Unit] = Free.eval(CounterExpr.Increment(n))
    def decrement(n: Int): Counter[Nothing, Unit] = Free.eval(CounterExpr.Decrement(n))
    def get(): Counter[Nothing, Int]              = Free.eval(CounterExpr.Get())
    def overflow(target: Int): Counter[CounterError.Overflow, Nothing] =
      Free.eval(CounterExpr.Overflow(target))
  }

  object interpreters {
    import grammar.*
    import grammar.CounterExpr.*
    import dsl.Counter

    def unsafeInterpreter(initialValue: Int): Free.UnsafeInterpreter[CounterExpr] = {
      var counter = initialValue
      new Free.UnsafeInterpreter[CounterExpr] {
        def interpret[E, A](fa: CounterExpr[E, A]): Either[E, A] = fa match {
          case Increment(n) =>
            counter += n
            Right(())
          case Decrement(n) =>
            counter -= n
            Right(())
          case Get() =>
            Right(counter)
          case Overflow(target) =>
            Left(CounterError.Overflow(counter, target))
        }
      }
    }
  }
}
