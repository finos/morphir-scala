package morphir.appkit.electron

import kyo.*

/**
 * A message-oriented string channel between two Electron processes. The Electron IPC layer is already message-framed,
 * so the seam carries whole messages, not bytes.
 */
trait IpcPort:
  def send(message: String): Unit < (Async & Abort[Closed])
  def incoming: Stream[String, Async & Abort[Closed]]
  def close: Unit < Async

object IpcPort:

  /** Cross-wired in-memory pair for tests: a.send arrives on b.incoming and vice versa. */
  def inMemoryPair(capacity: Int = 64): (IpcPort, IpcPort) < Sync =
    for
      aToB <- Channel.initUnscoped[String](capacity)
      bToA <- Channel.initUnscoped[String](capacity)
    yield (channelPort(out = aToB, in = bToA), channelPort(out = bToA, in = aToB))

  private def channelPort(out: Channel[String], in: Channel[String]): IpcPort =
    new IpcPort:
      def send(message: String): Unit < (Async & Abort[Closed]) = out.put(message)
      def incoming: Stream[String, Async & Abort[Closed]]       = in.stream()
      def close: Unit < Async                                   = out.close.unit.andThen(in.close.unit)
