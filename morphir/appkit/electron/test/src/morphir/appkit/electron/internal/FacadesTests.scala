package morphir.appkit.electron.internal

import kyo.*
import kyo.test.*
import scala.scalajs.js as sjs

class FacadesTests extends Test[Any]:

  "facades.awaitPromise" - {

    "captures a resolved Promise" in
      facades.awaitPromise(sjs.Promise.resolve("ready")).map { result =>
        assert(result == Result.Success("ready"))
      }

    "captures a rejected Promise" in {
      val rejection = new RuntimeException("rejected")
      facades.awaitPromise(sjs.Promise.reject(rejection)).map {
        case Result.Failure(error) => assert(error eq rejection)
        case _                     => assert(false)
      }
    }

    "captures a synchronously throwing Promise expression" in {
      val failure = new RuntimeException("thrown before Promise construction")
      facades.awaitPromise[String](throw failure).map {
        case Result.Failure(error) => assert(error eq failure)
        case _                     => assert(false)
      }
    }
  }
end FacadesTests
