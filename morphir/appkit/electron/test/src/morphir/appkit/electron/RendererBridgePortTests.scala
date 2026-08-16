package morphir.appkit.electron

import kyo.*
import kyo.test.*
import scala.scalajs.js as sjs

class RendererBridgePortTests extends Test[Any]:

  final class FakeBridge extends IpcBridge:
    val sent                                            = scala.collection.mutable.ListBuffer.empty[String]
    var handler: sjs.Function1[String, Unit]            = null
    def postMessage(message: String): Unit              = sent += message
    def onMessage(h: sjs.Function1[String, Unit]): Unit = handler = h

  "ElectronPorts.rendererPort" - {

    "sends through the bridge and surfaces bridge messages on incoming" in {
      val bridge = new FakeBridge
      ElectronPorts.rendererPort(bridge).map { port =>
        port.send("hello").andThen {
          bridge.handler("world")
          port.incoming.take(1).run.map { received =>
            assert(bridge.sent.toList == List("hello") && received == Chunk("world"))
          }
        }
      }
    }
  }
end RendererBridgePortTests
