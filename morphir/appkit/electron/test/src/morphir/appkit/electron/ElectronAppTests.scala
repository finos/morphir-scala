package morphir.appkit.electron

import kyo.*
import kyo.test.*
import scala.scalajs.js as sjs

class ElectronAppTests extends Test[Any]:

  "ElectronApp window options" - {
    "keep isolation, sandboxing, and Node integration explicit when preload is present" in {
      val options = ElectronApp.windowOptions(
        ElectronApp.WindowOptions(preloadPath = Present("/app/preload.cjs"))
      ).asInstanceOf[sjs.Dynamic]
      val preferences = options.webPreferences.asInstanceOf[sjs.Dynamic]

      assert(preferences.contextIsolation.asInstanceOf[Boolean])
      assert(preferences.sandbox.asInstanceOf[Boolean])
      assert(!preferences.nodeIntegration.asInstanceOf[Boolean])
      assert(preferences.preload.asInstanceOf[String] == "/app/preload.cjs")
    }
  }
end ElectronAppTests
