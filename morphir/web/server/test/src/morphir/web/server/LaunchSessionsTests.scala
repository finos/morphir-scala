package morphir.web.server

import kyo.*
import kyo.test.*

class LaunchSessionsTests extends Test[Any]:

  private final class DeterministicEntropy(values: List[Array[Byte]]):
    private val remaining = scala.collection.mutable.Queue.from(values)
    var requestedLengths  = List.empty[Int]

    val random: SecureRandom =
      import AllowUnsafe.embrace.danger
      SecureRandom(new SecureRandom.Unsafe:
        def nextBytes(length: Int)(using AllowUnsafe): Span[Byte] = synchronized {
          requestedLengths = requestedLengths :+ length
          val next = remaining.dequeue()
          require(next.length == length)
          Span.from(next)
        })

  private def bytes(seed: Int): Array[Byte] = Array.tabulate(32)(i => (seed + i).toByte)

  "LaunchSessions" - {

    "creates 256-bit URL-safe values and atomically exchanges a launch once" in {
      val entropy = DeterministicEntropy(List(bytes(1), bytes(65), bytes(97)))
      SecureRandom.let(entropy.random) {
        Clock.withTimeControl { _ =>
          LaunchSessions.init(1.minute).map { sessions =>
            sessions.createLaunch.map { launch =>
              sessions.exchange(launch.value).map { first =>
                sessions.exchange(launch.value).map { replay =>
                  first match
                    case Result.Success(cookie) =>
                      val rawSession = cookie.headerValue.stripPrefix("morphir_session=").takeWhile(_ != ';')
                      sessions.authenticate(rawSession).map { authenticated =>
                        assert(entropy.requestedLengths == List(32, 32, 32))
                        assert(launch.value.matches("[A-Za-z0-9_-]{43}"))
                        assert(cookie.headerValue == s"morphir_session=$rawSession; HttpOnly; SameSite=Strict; Path=/")
                        assert(!cookie.headerValue.contains("; Secure"))
                        assert(authenticated)
                        assert(replay == Result.fail(LaunchSessions.ExchangeError.InvalidLaunch))
                      }
                    case _ => assert(false)
                }
              }
            }
          }
        }
      }
    }

    "rejects an expired launch without establishing a session" in {
      val entropy = DeterministicEntropy(List(bytes(3), bytes(90)))
      SecureRandom.let(entropy.random) {
        Clock.withTimeControl { control =>
          LaunchSessions.init(1.minute).map { sessions =>
            sessions.createLaunch.map { launch =>
              control.advance(1.minute).map { _ =>
                sessions.exchange(launch.value).map { result =>
                  assert(result == Result.fail(LaunchSessions.ExchangeError.ExpiredLaunch))
                }
              }
            }
          }
        }
      }
    }

    "expires the current session" in {
      val entropy = DeterministicEntropy(List(bytes(4), bytes(80)))
      SecureRandom.let(entropy.random) {
        Clock.withTimeControl { control =>
          LaunchSessions.init(1.minute).map { sessions =>
            sessions.createLaunch.map { launch =>
              sessions.exchange(launch.value).map {
                case Result.Success(cookie) =>
                  val rawSession = cookie.headerValue.stripPrefix("morphir_session=").takeWhile(_ != ';')
                  control.advance(1.minute).map { _ =>
                    sessions.authenticate(rawSession).map(authenticated => assert(!authenticated))
                  }
                case _ => assert(false)
              }
            }
          }
        }
      }
    }

    "routes digest checks through the constant-time comparer" in {
      val entropy                                         = DeterministicEntropy(List(bytes(5), bytes(100)))
      var comparisons                                     = 0
      val comparer: (Array[Byte], Array[Byte]) => Boolean = (left, right) =>
        comparisons += 1
        java.security.MessageDigest.isEqual(left, right)

      SecureRandom.let(entropy.random) {
        Clock.withTimeControl { _ =>
          LaunchSessions.init(1.minute, comparer).map { sessions =>
            sessions.createLaunch.map { launch =>
              sessions.exchange(launch.value).map { exchanged =>
                assert(exchanged.isSuccess && comparisons > 0)
              }
            }
          }
        }
      }
    }

    "allows exactly one concurrent exchange of the same launch" in {
      val entropy = DeterministicEntropy(List(bytes(6), bytes(110), bytes(120)))
      SecureRandom.let(entropy.random) {
        Clock.withTimeControl { _ =>
          Latch.init(2).map { ready =>
            Latch.init(1).map { start =>
              LaunchSessions.init(1.minute).map { sessions =>
                sessions.createLaunch.map { launch =>
                  def exchange = ready.release.andThen(start.await).andThen(sessions.exchange(launch.value))
                  for
                    first  <- Fiber.init(exchange)
                    second <- Fiber.init(exchange)
                    _      <- ready.await
                    _      <- start.release
                    one    <- first.get
                    two    <- second.get
                  yield assert(
                    Seq(one, two).count(_.isSuccess) == 1 &&
                      Seq(one, two).count(_ == Result.fail(LaunchSessions.ExchangeError.InvalidLaunch)) == 1
                  )
                }
              }
            }
          }
        }
      }
    }

    "replaces an unused launch when recovery mints a newer credential" in {
      val entropy = DeterministicEntropy(List(bytes(7), bytes(40), bytes(80), bytes(120)))
      SecureRandom.let(entropy.random) {
        Clock.withTimeControl { _ =>
          LaunchSessions.init(1.minute).map { sessions =>
            sessions.createLaunch.map { oldLaunch =>
              sessions.createLaunch.map { freshLaunch =>
                sessions.exchange(oldLaunch.value).map { oldResult =>
                  sessions.exchange(freshLaunch.value).map { freshResult =>
                    assert(oldResult == Result.fail(LaunchSessions.ExchangeError.InvalidLaunch))
                    assert(freshResult.isSuccess)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
end LaunchSessionsTests
