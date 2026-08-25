package morphir.desktop.smoke

import kyo.test.*
import scala.scalajs.{js => sjs}

class SmokeDriverTests extends Test[Any]:

  private val expectedNames = List(
    "clearedAfterFailure",
    "clearedAfterSessionSuccess",
    "clearedAfterSuccess",
    "disconnectedThroughButton",
    "mountedRenderer",
    "rememberFalseReadLive",
    "rememberReadLive",
    "rememberTrueReadLive",
    "removedStoredCredentialThroughButton",
    "rendererConsoleSentinelFree",
    "retainedOnFailure",
    "retainedOnSessionSuccess",
    "retainedOnSuccess",
    "safeConnectedStatus",
    "safeRejectedError",
    "safeSessionStatus",
    "submittedThroughForm",
    "transientDomSentinelFree"
  )

  "SmokeDriver assertion contract" - {

    "declares the exact DesktopSmoke assertion names in stable order" in {
      assert(SmokeDriver.assertionNames == expectedNames)
      assert(SmokeDriver.assertionNames.toSet == expectedNames.toSet)
      assert(SmokeDriver.assertionNames.distinct.size == expectedNames.size)
    }

    "assembles one flat JavaScript dictionary in assertion order" in {
      val values = expectedNames.indices.map(index => index % 2 == 0).toList
      val result = SmokeDriver.assembleAssertions(values)

      assert(sjs.Object.keys(result.asInstanceOf[sjs.Object]).toList == expectedNames)
      assert(expectedNames.zip(values).forall { case (name, value) => result(name) == value })
    }
  }

  "SmokeDriver safe text checks" - {

    "accept required safe copy when the secret is absent" in {
      val text = "Connected as smoke-user Connected and remembered on this device."

      assert(
        SmokeDriver.containsSafeText(
          text,
          List("Connected as smoke-user", "Connected and remembered on this device."),
          "test-secret"
        )
      )
    }

    "reject text containing the secret even when required safe copy is present" in {
      val secret = "test-secret"
      val text   = s"GitHub rejected this token. $secret"

      assert(!SmokeDriver.containsSafeText(text, List("GitHub rejected this token."), secret))
    }

    "reject session status when the live remember-false observation failed" in {
      val text = "Connected for this session. Connected and remembered on this device."

      assert(!SmokeDriver.sessionStatusIsSafe(rememberFalseReadLive = false, text, "test-secret"))
    }
  }
end SmokeDriverTests
