package morphir.appkit.electron

import kyo.*

/**
 * Lifts a message-oriented [[IpcPort]] into an envelope-level [[kyo.JsonRpcTransport]] by encoding each
 * [[kyo.JsonRpcEnvelope]] as one JSON message. No framer: Electron IPC delivers whole messages.
 */
object ElectronIpcTransport:

  def fromPort(port: IpcPort): JsonRpcTransport =
    new JsonRpcTransport:
      def send(env: JsonRpcEnvelope)(using Frame): Unit < (Async & Abort[Closed]) =
        port.send(Json.encode(env))
      def incoming(using Frame): Stream[JsonRpcEnvelope, Async & Abort[Closed]] =
        port.incoming.map { message =>
          Json.decode[JsonRpcEnvelope](message) match
            case Result.Success(env) => env
            case Result.Failure(err) =>
              Abort.panic(new IllegalStateException(s"malformed JSON-RPC frame: $err"))
            case Result.Panic(ex) => Abort.panic(ex)
        }
      def close(using Frame): Unit < Async =
        port.close
