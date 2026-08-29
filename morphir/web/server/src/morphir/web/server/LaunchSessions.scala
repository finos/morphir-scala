package morphir.web.server

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64
import kyo.*

final class LaunchSessions private (
    ttl: Duration,
    compareDigests: (Array[Byte], Array[Byte]) => Boolean,
    clock: Clock
):

  import LaunchSessions.*

  private final case class Launch(digest: Array[Byte], expiresAt: Instant)
  private final case class Session(digest: Array[Byte], expiresAt: Instant)

  private val lock                           = new Object
  private var launches                       = Vector.empty[Launch]
  private var currentSession: Maybe[Session] = Absent

  def createLaunch(using Frame): LaunchCredential < Sync =
    clock.now.map { now =>
      SecureRandom.nextBytes(entropyBytes).map { entropy =>
        val raw = encode(entropy.toArray)
        Sync.defer {
          lock.synchronized {
            launches = Vector(Launch(digest(raw), now + ttl))
          }
          LaunchCredential(raw)
        }
      }
    }

  def exchange(rawLaunch: String)(using Frame): Result[ExchangeError, SessionCookie] < Sync =
    clock.now.map { now =>
      SecureRandom.nextBytes(entropyBytes).map { entropy =>
        val rawSession    = encode(entropy.toArray)
        val launchDigest  = digest(rawLaunch)
        val sessionDigest = digest(rawSession)
        Sync.defer {
          lock.synchronized {
            val index = launches.indexWhere(entry => compareDigests(entry.digest, launchDigest))
            if index < 0 then Result.fail(ExchangeError.InvalidLaunch)
            else
              val launch = launches(index)
              launches = launches.patch(index, Nil, 1)
              if now >= launch.expiresAt then Result.fail(ExchangeError.ExpiredLaunch)
              else
                currentSession = Present(Session(sessionDigest, now + ttl))
                Result.succeed(SessionCookie(cookie(rawSession)))
          }
        }
      }
    }

  def authenticate(rawSession: String)(using Frame): Boolean < Sync =
    clock.now.map { now =>
      val candidate = digest(rawSession)
      Sync.defer {
        lock.synchronized {
          currentSession.exists(session => now < session.expiresAt && compareDigests(session.digest, candidate))
        }
      }
    }

  override def toString: String = "LaunchSessions(<redacted>)"

object LaunchSessions:

  val entropyBytes = 32

  enum ExchangeError derives CanEqual:
    case InvalidLaunch, ExpiredLaunch

  final class LaunchCredential private[LaunchSessions] (private val raw: String):
    def value: String             = raw
    override def toString: String = "LaunchCredential(<redacted>)"

  final class SessionCookie private[LaunchSessions] (val headerValue: String):
    override def toString: String = "SessionCookie(<redacted>)"

  def init(ttl: Duration)(using Frame): LaunchSessions < Sync =
    init(ttl, MessageDigest.isEqual)

  private[server] def init(
      ttl: Duration,
      compareDigests: (Array[Byte], Array[Byte]) => Boolean
  )(using Frame): LaunchSessions < Sync =
    Clock.get.map { clock =>
      require(ttl != Duration.Infinity && ttl > Duration.Zero, "session TTL must be finite and positive")
      new LaunchSessions(ttl, compareDigests, clock)
    }

  private def encode(bytes: Array[Byte]): String =
    Base64.getUrlEncoder.withoutPadding().encodeToString(bytes)

  private def digest(value: String): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8))

  private def cookie(rawSession: String): String =
    s"morphir_session=$rawSession; HttpOnly; SameSite=Strict; Path=/"
end LaunchSessions
