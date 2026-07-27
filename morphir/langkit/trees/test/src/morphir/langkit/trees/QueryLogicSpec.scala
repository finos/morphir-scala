package morphir.langkit.trees

import kyo.test.*

import morphir.langkit.trees.query.QueryLogic

class QueryLogicSpec extends Test[Any]:

  private type Ctx = Int
  private type Log = String
  private type Err = String

  "QueryLogic" - {
    "threads and updates context" in {
      val result = QueryLogic.run[Ctx, Log, Err, Int](initialContext = 2) {
        for
          start <- QueryLogic.readContext[Ctx, Log, Err]
          _     <- QueryLogic.updateContext[Ctx, Log, Err](_ + 5)
          end   <- QueryLogic.readContext[Ctx, Log, Err]
        yield end - start
      }
      assert(result.value == Right(5))
      assert(result.context == 7)
    }
    "accumulates logs in order" in {
      val result = QueryLogic.run[Ctx, Log, Err, Unit](initialContext = 0) {
        for
          _ <- QueryLogic.log[Ctx, Log, Err]("first")
          _ <- QueryLogic.log[Ctx, Log, Err]("second")
        yield ()
      }
      assert(result.logs == Vector("first", "second"))
    }
    "accumulates errors while continuing" in {
      val result = QueryLogic.run[Ctx, Log, Err, Int](initialContext = 3) {
        for
          _   <- QueryLogic.error[Ctx, Log, Err]("bad-a")
          _   <- QueryLogic.error[Ctx, Log, Err]("bad-b")
          ctx <- QueryLogic.readContext[Ctx, Log, Err]
        yield ctx
      }
      assert(result.value == Left(Vector("bad-a", "bad-b")))
      assert(result.context == 3)
    }
    "returns successful value when no errors were emitted" in {
      val result = QueryLogic.run[Ctx, Log, Err, Int](initialContext = 10) {
        for
          _   <- QueryLogic.log[Ctx, Log, Err]("ok")
          ctx <- QueryLogic.readContext[Ctx, Log, Err]
        yield ctx
      }
      assert(result.value == Right(10))
      assert(result.logs == Vector("ok"))
      assert(result.errors.isEmpty)
    }
    "setContext replaces threaded context" in {
      val result = QueryLogic.run[Ctx, Log, Err, Int](initialContext = 1) {
        for
          _   <- QueryLogic.setContext[Ctx, Log, Err](99)
          ctx <- QueryLogic.readContext[Ctx, Log, Err]
        yield ctx
      }
      assert(result.value == Right(99))
      assert(result.context == 99)
    }
    "failFast aborts and surfaces the error in the result envelope" in {
      val result = QueryLogic.run[Ctx, Log, Err, Int](initialContext = 0) {
        for _ <- QueryLogic.failFast[Ctx, Log, Err]("fatal")
        yield 42
      }
      assert(result.value == Left(Vector("fatal")))
      assert(result.errors == Vector("fatal"))
    }
    "failFast after soft errors merges both into the result envelope" in {
      val result = QueryLogic.run[Ctx, Log, Err, Int](initialContext = 0) {
        for
          _ <- QueryLogic.error[Ctx, Log, Err]("soft-a")
          _ <- QueryLogic.failFast[Ctx, Log, Err]("fatal")
        yield 42
      }
      assert(result.value == Left(Vector("soft-a", "fatal")))
      assert(result.errors == Vector("soft-a", "fatal"))
    }
  }
