package morphir.langkit.itest.steps

import io.cucumber.scala.{EN, ScalaDsl}

import morphir.langkit.elm.compiler.abi.InvokeJson
import morphir.langkit.elm.compiler.abi.InvokeResponse
import morphir.langkit.itest.TestDriver

class CompilerApiSteps extends ScalaDsl with EN:

  import InvokeJson.decode
  import InvokeJson.given

  private var backend: Option[String]         = None
  private var inputJson: Option[String]       = None
  private var responses: Vector[String]       = Vector.empty
  private var decoded: Option[InvokeResponse] = None

  Given("the compiler backend {string}") { (name: String) =>
    val normalized = name.trim.toLowerCase
    TestDriver.requireSupportedBackend(normalized)
    backend = Some(normalized)
    responses = Vector.empty
    decoded = None
  }

  Given("the compiler input:") { (json: String) =>
    inputJson = Some(json)
    responses = Vector.empty
    decoded = None
  }

  When("compiler operation {string} is invoked") { (op: String) =>
    val response = invoke(op)
    responses = Vector(response)
    decoded = Some(decode[InvokeResponse](response))
  }

  When("compiler operation {string} is invoked twice") { (op: String) =>
    val first  = invoke(op)
    val second = invoke(op)
    responses = Vector(first, second)
    decoded = Some(decode[InvokeResponse](first))
  }

  Then("the compiler response is ok") { () =>
    val response = currentResponse()
    assert(response.ok, s"expected compiler response to be ok, got: ${responses.headOption.getOrElse("<none>")}")
  }

  Then("the compiler response is not ok") { () =>
    val response = currentResponse()
    assert(
      !response.ok,
      s"expected compiler response not to be ok, got: ${responses.headOption.getOrElse("<none>")}"
    )
  }

  Then("the compiler response value contains {string}") { (needle: String) =>
    val value = currentResponse().value.getOrElse(
      throw new AssertionError(
        s"expected compiler response value, got: ${responses.headOption.getOrElse("<none>")}"
      )
    )
    assert(value.contains(needle), s"expected compiler response value to contain [$needle], got: $value")
  }

  Then("the compiler response has at least {int} error(s)") { (count: Int) =>
    val actual = currentResponse().errors.size
    assert(actual >= count, s"expected at least $count compiler error(s), got $actual")
  }

  Then("compiler error {int} phase is {string}") { (oneBasedIndex: Int, phase: String) =>
    val error = compilerError(oneBasedIndex)
    assert(error.phase == phase, s"expected compiler error $oneBasedIndex phase [$phase], got [${error.phase}]")
  }

  Then("compiler error {int} message contains {string}") { (oneBasedIndex: Int, needle: String) =>
    val error = compilerError(oneBasedIndex)
    assert(
      error.message.contains(needle),
      s"expected compiler error $oneBasedIndex message to contain [$needle], got: ${error.message}"
    )
  }

  Then("the compiler responses are byte-identical") { () =>
    responses match
      case Vector(first, second) =>
        assert(first == second, s"expected byte-identical compiler responses.\nFirst:  $first\nSecond: $second")
      case other =>
        throw new AssertionError(s"expected exactly two compiler responses, got ${other.size}")
  }

  private def invoke(op: String): String =
    val json = inputJson.getOrElse(throw new AssertionError("compiler input not set - missing Given step?"))
    TestDriver.invoke(
      backend = backend.getOrElse(throw new AssertionError("compiler backend not set - missing Given step?")),
      op = op,
      inputJson = json
    )

  private def currentResponse(): InvokeResponse =
    decoded.getOrElse(throw new AssertionError("compiler response not set - missing When step?"))

  private def compilerError(oneBasedIndex: Int) =
    currentResponse().errors
      .lift(oneBasedIndex - 1)
      .getOrElse(
        throw new AssertionError(s"compiler error $oneBasedIndex not found in ${currentResponse().errors}")
      )
