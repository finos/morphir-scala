package morphir.appkit.electron

import kyo.*
import kyo.test.*

class ElectronIpcTransportTests extends Test[Any]:

  final case class EchoReq(text: String) derives CanEqual, Schema
  final case class EchoResp(text: String) derives CanEqual, Schema

  val echo = JsonRpcRoute.request[EchoReq, EchoResp]("echo") { (req, _) =>
    EchoResp(req.text.reverse)
  }

  "ElectronIpcTransport" - {

    "carries a full JSON-RPC round-trip over an IpcPort pair" in
      IpcPort.inMemoryPair().map { (mainPort, rendererPort) =>
        val serverTransport = ElectronIpcTransport.fromPort(mainPort)
        val clientTransport = ElectronIpcTransport.fromPort(rendererPort)
        JsonRpcHandler.init(serverTransport, echo).map { _ =>
          JsonRpcHandler.init(clientTransport).map { client =>
            client
              .call[EchoReq, EchoResp]("echo", EchoReq("morphir"))
              .map(resp => assert(resp.text == "rihprom"))
          }
        }
      }
  }
end ElectronIpcTransportTests
