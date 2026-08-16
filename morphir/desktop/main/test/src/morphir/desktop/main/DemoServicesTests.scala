package morphir.desktop.main

import kyo.*
import kyo.test.*
import morphir.ui.services.*

class DemoServicesTests extends Test[Any]:

  val ws = WorkspaceRef("/demo")

  "DemoServices" - {

    "ir.listPackages includes the Morphir SDK" in
      DemoServices.ir.listPackages(ws).map { packages =>
        assert(packages.exists(_.name == "Morphir.SDK"))
      }

    "knowledge.intentIndex includes this app's intent" in
      DemoServices.knowledge.intentIndex(ws).map { intents =>
        assert(intents.exists(_.number == "0030"))
      }

    "shell.appVersion reports the injected version" in
      DemoServices.shell("9.9.9").appVersion().map(v => assert(v == "9.9.9"))

    "routes cover all three services" in {
      val names = DemoServices.routes("9.9.9").map(_.name)
      assert(
        names.contains(IrRpc.methods.listPackages) &&
          names.contains(KnowledgeRpc.methods.intentIndex) &&
          names.contains(ShellRpc.methods.appVersion)
      )
    }
  }
end DemoServicesTests
