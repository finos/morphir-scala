package morphir.ui.github

import kyo.*
import kyo.test.*
import morphir.appkit.*
import morphir.connector.github.*
import morphir.ui.services.*

class GitHubConnectionCoordinatorTests extends Test[Any]:

  private val firstRaw  = "coordinator-first-token"
  private val secondRaw = "coordinator-second-token"
  private val first     = token(firstRaw)
  private val second    = token(secondRaw)
  private val ada       = gitHubLogin"ada-lovelace"
  private val grace     = gitHubLogin"grace-hopper"

  private def token(raw: String): Token =
    Token.parse(raw) match
      case Present(value) => value
      case Absent         => throw new AssertionError("test token must be non-blank")

  private def secret(raw: String): Secret =
    Secret.fromStored(raw) match
      case Present(value) => value
      case Absent         => throw new AssertionError("test secret must be non-empty")

  private def tokenText(codePoints: Int*): String =
    codePoints.iterator.map(_.toChar).mkString

  private def verifier(
      verifyToken: Token => GitHubLogin < (Abort[GitHubException] & Async)
  ): GitHubTokenVerifier =
    new GitHubTokenVerifier:
      def verify(token: Token): GitHubLogin < (Abort[GitHubException] & Async) = verifyToken(token)

  private def accepts(entries: (Token, GitHubLogin)*): GitHubTokenVerifier =
    verifier { candidate =>
      entries.find((token, _) => token == candidate) match
        case Some((_, login)) => login
        case None             => Abort.fail(GitHubException.Unauthorized("test rejection"))
    }

  private def run[A](effect: A < (Async & Abort[GitHubConnectionError])): Result[GitHubConnectionError, A] < Async =
    Abort.run[GitHubConnectionError](effect)

  private def readProvider(coordinator: GitHubConnectionCoordinator): Result[GitHubException, Token] < Async =
    Abort.run[GitHubException](coordinator.tokenProvider.token)

  private def awaitWithin[A](effect: => A < Async): Result[Timeout, A] < Async =
    Abort.run[Timeout](Async.timeout(2.seconds)(effect))

  private def useTargetFiber[A](effect: => A < Async)(use: Fiber[A, Any] => Unit < Async): Unit < Async =
    Fiber.use[Nothing, A, Any, Any](effect)[Unit, Async](use)

  private def awaitStatus(
      coordinator: GitHubConnectionCoordinator,
      expected: GitHubConnectionStatus
  ): Unit < Async =
    run(coordinator.status()).map {
      case Result.Success(status) if status == expected => ()
      case _ => Async.sleep(1.millis).andThen(awaitStatus(coordinator, expected))
    }

  private enum TransitionRace:
    case ContentionObserved, CompetingOperationCompleted

  private def awaitContentionOrCompletion(
      contentionObserved: Latch,
      competingOperationCompleted: Latch
  ): TransitionRace < Async =
    Async.raceFirst(
      contentionObserved.await.andThen(TransitionRace.ContentionObserved),
      competingOperationCompleted.await.andThen(TransitionRace.CompetingOperationCompleted)
    )

  "GitHubConnectionCoordinator startup" - {
    "starts disconnected when the vault has no credential" in {
      val vault = MemoryVault()
      run(GitHubConnectionCoordinator.init(accepts(first -> ada), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.status()).map { status =>
            assert(status == Result.succeed(GitHubConnectionStatus.Disconnected))
            readProvider(coordinator).map {
              case Result.Failure(_: GitHubException.Unauthorized) => assert(true)
              case _                                               => assert(false)
            }
          }
        case _ => assert(false)
      }
    }

    "validates a stored credential once and activates it" in {
      val vault             = MemoryVault(Present(secret(firstRaw)))
      var firstVerification = true
      val once              = verifier { _ =>
        if firstVerification then
          firstVerification = false
          ada
        else Abort.fail(GitHubException.Unauthorized("must not verify status again"))
      }

      run(GitHubConnectionCoordinator.init(once, Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.status()).map { firstStatus =>
            run(coordinator.status()).map { secondStatus =>
              readProvider(coordinator).map { provider =>
                val expected = GitHubConnectionStatus.Connected(ada.value, ConnectionPersistence.Device)
                assert(firstStatus == Result.succeed(expected))
                assert(secondStatus == Result.succeed(expected))
                assert(provider == Result.succeed(first))
              }
            }
          }
        case _ => assert(false)
      }
    }

    "reports a rejected stored credential without activating it" in {
      val vault   = MemoryVault(Present(secret(firstRaw)))
      val rejects = verifier(_ => Abort.fail(GitHubException.Unauthorized("stored credential rejected")))

      run(GitHubConnectionCoordinator.init(rejects, Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.status()).map { status =>
            readProvider(coordinator).map { provider =>
              assert(status == Result.succeed(GitHubConnectionStatus.StoredCredentialRejected))
              assert(provider.isFailure)
            }
          }
        case _ => assert(false)
      }
    }

    "continues startup when a stored credential cannot be verified" in {
      val vault   = MemoryVault(Present(secret(firstRaw)))
      val offline = verifier(_ => Abort.fail(GitHubException.Transport("offline")))

      run(GitHubConnectionCoordinator.init(offline, Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.status()).map { status =>
            assert(status == Result.succeed(GitHubConnectionStatus.Disconnected))
            assert(vault.snapshot == Present(secret(firstRaw)))
          }
        case _ => assert(false)
      }
    }

    "continues startup when stored-token verification panics" in {
      val vault  = MemoryVault(Present(secret(firstRaw)))
      val panics = verifier(_ => Sync.defer(throw new RuntimeException("verification panic")))

      run(GitHubConnectionCoordinator.init(panics, Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.status()).map { status =>
            assert(status == Result.succeed(GitHubConnectionStatus.Disconnected))
            assert(vault.snapshot == Present(secret(firstRaw)))
          }
        case _ => assert(false)
      }
    }

    "keeps session connections available when secure storage is unavailable" in {
      val vault = MemoryVault()
      vault.rejectGet(SecretException.NotAvailable("unavailable-secret-sentinel"))
      vault.rejectPut(SecretException.NotAvailable("unavailable-secret-sentinel"))

      run(GitHubConnectionCoordinator.init(accepts(first -> ada, second -> grace), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.status()).map { initialStatus =>
            run(coordinator.connect(TokenSubmission.from(firstRaw), remember = false)).map { connected =>
              run(coordinator.connect(TokenSubmission.from(secondRaw), remember = true)).map { remembered =>
                run(coordinator.status()).map { finalStatus =>
                  val expected = GitHubConnectionStatus.Connected(ada.value, ConnectionPersistence.Session)
                  assert(initialStatus == Result.succeed(GitHubConnectionStatus.Disconnected))
                  assert(connected == Result.succeed(expected))
                  assert(remembered == Result.fail(GitHubConnectionError.SecureStorageUnavailable))
                  assert(finalStatus == Result.succeed(expected))
                  assert(!remembered.toString.contains("unavailable-secret-sentinel"))
                }
              }
            }
          }
        case _ => assert(false)
      }
    }

    "keeps session connections available when secure storage lookup fails" in {
      val vault = MemoryVault()
      vault.rejectGet(SecretException.LookupFailed("lookup-secret-sentinel"))
      vault.rejectPut(SecretException.MutationFailed("lookup-secret-sentinel"))

      run(GitHubConnectionCoordinator.init(accepts(first -> ada, second -> grace), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.status()).map { initialStatus =>
            run(coordinator.connect(TokenSubmission.from(firstRaw), remember = false)).map { connected =>
              run(coordinator.connect(TokenSubmission.from(secondRaw), remember = true)).map { remembered =>
                run(coordinator.status()).map { finalStatus =>
                  val expected = GitHubConnectionStatus.Connected(ada.value, ConnectionPersistence.Session)
                  assert(initialStatus == Result.succeed(GitHubConnectionStatus.Disconnected))
                  assert(connected == Result.succeed(expected))
                  assert(remembered == Result.fail(GitHubConnectionError.SecureStorageFailure))
                  assert(finalStatus == Result.succeed(expected))
                  assert(!remembered.toString.contains("lookup-secret-sentinel"))
                }
              }
            }
          }
        case _ => assert(false)
      }
    }
  }

  "GitHubConnectionCoordinator connect" - {
    "removes an unread stored credential before accepting a session replacement" in {
      val vault = MemoryVault(Present(secret(firstRaw)))
      vault.rejectGet(SecretException.LookupFailed("offline"))

      run(GitHubConnectionCoordinator.init(accepts(second -> grace), Present(vault))).map {
        case Result.Success(coordinator) =>
          vault.allowGet()
          run(coordinator.connect(TokenSubmission.from(secondRaw), remember = false)).map { connected =>
            assert(
              connected == Result.succeed(
                GitHubConnectionStatus.Connected(grace.value, ConnectionPersistence.Session)
              )
            )
            assert(vault.snapshot.isEmpty)
          }
        case _ => assert(false)
      }
    }

    "does not activate a session replacement while an unread credential cannot be removed" in {
      val vault = MemoryVault(Present(secret(firstRaw)))
      vault.rejectGet(SecretException.LookupFailed("offline"))
      vault.rejectRemove(SecretException.MutationFailed("offline"))

      run(GitHubConnectionCoordinator.init(accepts(second -> grace), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(secondRaw), remember = false)).map { connected =>
            run(coordinator.status()).map { status =>
              assert(connected == Result.fail(GitHubConnectionError.SecureStorageFailure))
              assert(status == Result.succeed(GitHubConnectionStatus.Disconnected))
              assert(vault.snapshot == Present(secret(firstRaw)))
            }
          }
        case _ => assert(false)
      }
    }

    "removes a rejected stored credential before activating its session replacement" in {
      val vault              = MemoryVault(Present(secret(firstRaw)))
      val acceptsReplacement = accepts(second -> grace)

      run(GitHubConnectionCoordinator.init(acceptsReplacement, Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(secondRaw), remember = false)).map { connected =>
            run(coordinator.disconnect()).map { disconnected =>
              run(GitHubConnectionCoordinator.init(acceptsReplacement, Present(vault))).map {
                case Result.Success(restarted) =>
                  run(restarted.status()).map { restartedStatus =>
                    readProvider(restarted).map { restartedProvider =>
                      assert(
                        connected == Result.succeed(
                          GitHubConnectionStatus.Connected(grace.value, ConnectionPersistence.Session)
                        )
                      )
                      assert(disconnected == Result.succeed(()))
                      assert(vault.snapshot.isEmpty)
                      assert(restartedStatus == Result.succeed(GitHubConnectionStatus.Disconnected))
                      assert(restartedProvider.isFailure)
                    }
                  }
                case _ => assert(false)
              }
            }
          }
        case _ => assert(false)
      }
    }

    "preserves a remembered connection when removing it for a session replacement fails" in {
      val vault = MemoryVault()

      run(GitHubConnectionCoordinator.init(accepts(first -> ada, second -> grace), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(firstRaw), remember = true)).map { _ =>
            vault.rejectRemove(SecretException.MutationFailed("remove-secret-sentinel"))
            run(coordinator.connect(TokenSubmission.from(secondRaw), remember = false)).map { replacement =>
              run(coordinator.status()).map { status =>
                readProvider(coordinator).map { provider =>
                  val expected = GitHubConnectionStatus.Connected(ada.value, ConnectionPersistence.Device)
                  assert(replacement == Result.fail(GitHubConnectionError.SecureStorageFailure))
                  assert(!replacement.toString.contains("remove-secret-sentinel"))
                  assert(status == Result.succeed(expected))
                  assert(provider == Result.succeed(first))
                  assert(vault.snapshot == Present(secret(firstRaw)))
                }
              }
            }
          }
        case _ => assert(false)
      }
    }

    "activates a session credential without storing it" in {
      val vault = MemoryVault()
      run(GitHubConnectionCoordinator.init(accepts(first -> ada), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(firstRaw), remember = false)).map { result =>
            readProvider(coordinator).map { provider =>
              assert(
                result == Result.succeed(
                  GitHubConnectionStatus.Connected(ada.value, ConnectionPersistence.Session)
                )
              )
              assert(provider == Result.succeed(first))
              assert(vault.snapshot.isEmpty)
            }
          }
        case _ => assert(false)
      }
    }

    "stores and activates a remembered credential" in {
      val vault = MemoryVault()
      run(GitHubConnectionCoordinator.init(accepts(first -> ada), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(firstRaw), remember = true)).map { result =>
            readProvider(coordinator).map { provider =>
              assert(
                result == Result.succeed(
                  GitHubConnectionStatus.Connected(ada.value, ConnectionPersistence.Device)
                )
              )
              assert(provider == Result.succeed(first))
              assert(vault.snapshot == Present(secret(firstRaw)))
            }
          }
        case _ => assert(false)
      }
    }

    "finishes activating a remembered credential when interrupted after storage" in
      Latch.init(1).map { credentialStored =>
        Latch.init(1).map { releaseVault =>
          val vault = MemoryVault()
          run(GitHubConnectionCoordinator.init(accepts(first -> ada, second -> grace), Present(vault))).map {
            case Result.Success(coordinator) =>
              run(coordinator.connect(TokenSubmission.from(firstRaw), remember = true)).map { _ =>
                vault.afterNextPut(credentialStored.release.andThen(releaseVault.await))
                Sync.ensure(releaseVault.release) {
                  useTargetFiber(
                    run(coordinator.connect(TokenSubmission.from(secondRaw), remember = true))
                  ) { connectionFiber =>
                    awaitWithin(credentialStored.await).map { entered =>
                      assert(entered.isSuccess)
                      connectionFiber.interrupt.map { _ =>
                        releaseVault.release.map { _ =>
                          awaitWithin(connectionFiber.getResult).map { interrupted =>
                            assert(
                              interrupted match
                                case Result.Panic(_: Interrupted) => true
                                case _                            => false
                            )
                            val expected =
                              GitHubConnectionStatus.Connected(grace.value, ConnectionPersistence.Device)
                            awaitWithin(awaitStatus(coordinator, expected)).map { completed =>
                              run(coordinator.status()).map { status =>
                                readProvider(coordinator).map { provider =>
                                  assert(completed.isSuccess)
                                  assert(vault.snapshot == Present(secret(secondRaw)))
                                  assert(status == Result.succeed(expected))
                                  assert(provider == Result.succeed(second))
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            case _ => assert(false)
          }
        }
      }

    "requires secure storage when remember is requested" in
      run(GitHubConnectionCoordinator.init(accepts(first -> ada), Absent)).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(firstRaw), remember = true)).map { result =>
            assert(result == Result.fail(GitHubConnectionError.SecureStorageUnavailable))
          }
        case _ => assert(false)
      }

    "maps validation rejection without exposing or activating the submission" in {
      val sentinel = tokenText(118, 97, 108, 105, 100, 97, 116, 105, 111, 110, 45, 114, 101, 106, 101, 99,
        116, 105, 111, 110, 45, 115, 101, 110, 116, 105, 110, 101, 108, 45, 116, 111, 107, 101, 110)
      val rejects = verifier(_ => Abort.fail(GitHubException.Unauthorized(sentinel)))

      run(GitHubConnectionCoordinator.init(rejects, Absent)).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(sentinel), remember = false)).map { result =>
            readProvider(coordinator).map { provider =>
              assert(result == Result.fail(GitHubConnectionError.RejectedToken))
              assert(!result.toString.contains(sentinel))
              assert(provider.isFailure)
            }
          }
        case _ => assert(false)
      }
    }

    "maps GitHub transport failures to a safe error" in {
      val unavailable = verifier(_ => Abort.fail(GitHubException.Transport("backend response detail")))

      run(GitHubConnectionCoordinator.init(unavailable, Absent)).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(firstRaw), remember = false)).map { result =>
            assert(result == Result.fail(GitHubConnectionError.GitHubUnavailable))
          }
        case _ => assert(false)
      }
    }

    "maps verifier panics to a safe error without exposing the throwable" in {
      val sentinel = tokenText(118, 101, 114, 105, 102, 105, 101, 114, 45, 112, 97, 110, 105, 99, 45, 115, 101,
        110, 116, 105, 110, 101, 108)
      val panics = verifier(_ => Sync.defer(throw new RuntimeException(sentinel)))

      run(GitHubConnectionCoordinator.init(panics, Absent)).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(firstRaw), remember = false)).map { result =>
            assert(result == Result.fail(GitHubConnectionError.GitHubUnavailable))
            assert(!result.toString.contains(sentinel))
            result match
              case Result.Failure(error) => assert(error.getMessage == "GitHub is unavailable. Try again.")
              case _                     => assert(false)
          }
        case _ => assert(false)
      }
    }

    "maps vault panics to a safe error and preserves the active connection" in {
      val sentinel = tokenText(118, 97, 117, 108, 116, 45, 112, 97, 110, 105, 99, 45, 115, 101, 110, 116, 105, 110,
        101, 108)
      val vault = MemoryVault()

      run(GitHubConnectionCoordinator.init(accepts(first -> ada, second -> grace), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(firstRaw), remember = false)).map { _ =>
            vault.panicPut(new RuntimeException(sentinel))
            run(coordinator.connect(TokenSubmission.from(secondRaw), remember = true)).map { result =>
              run(coordinator.status()).map { status =>
                readProvider(coordinator).map { provider =>
                  val expected = GitHubConnectionStatus.Connected(ada.value, ConnectionPersistence.Session)
                  assert(result == Result.fail(GitHubConnectionError.SecureStorageFailure))
                  assert(!result.toString.contains(sentinel))
                  result match
                    case Result.Failure(error) =>
                      assert(error.getMessage == "The credential could not be stored securely.")
                    case _ => assert(false)
                  assert(status == Result.succeed(expected))
                  assert(provider == Result.succeed(first))
                }
              }
            }
          }
        case _ => assert(false)
      }
    }

    "preserves an existing connection when validation fails" in {
      val vault          = MemoryVault()
      val validatesFirst = accepts(first -> ada)

      run(GitHubConnectionCoordinator.init(validatesFirst, Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(firstRaw), remember = false)).map { connected =>
            run(coordinator.connect(TokenSubmission.from(secondRaw), remember = false)).map { rejected =>
              run(coordinator.status()).map { status =>
                readProvider(coordinator).map { provider =>
                  assert(connected.isSuccess)
                  assert(rejected == Result.fail(GitHubConnectionError.RejectedToken))
                  assert(
                    status == Result.succeed(
                      GitHubConnectionStatus.Connected(ada.value, ConnectionPersistence.Session)
                    )
                  )
                  assert(provider == Result.succeed(first))
                }
              }
            }
          }
        case _ => assert(false)
      }
    }

    "preserves an existing connection when persistence fails" in {
      val vault = MemoryVault()
      run(GitHubConnectionCoordinator.init(accepts(first -> ada, second -> grace), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(firstRaw), remember = false)).map { connected =>
            vault.rejectPut(SecretException.MutationFailed("set"))
            run(coordinator.connect(TokenSubmission.from(secondRaw), remember = true)).map { rejected =>
              run(coordinator.status()).map { status =>
                readProvider(coordinator).map { provider =>
                  assert(connected.isSuccess)
                  assert(rejected == Result.fail(GitHubConnectionError.SecureStorageFailure))
                  assert(
                    status == Result.succeed(
                      GitHubConnectionStatus.Connected(ada.value, ConnectionPersistence.Session)
                    )
                  )
                  assert(provider == Result.succeed(first))
                }
              }
            }
          }
        case _ => assert(false)
      }
    }

    "replaces an existing connection after the new attempt succeeds" in {
      val vault = MemoryVault()
      run(GitHubConnectionCoordinator.init(accepts(first -> ada, second -> grace), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(firstRaw), remember = false)).map { _ =>
            run(coordinator.connect(TokenSubmission.from(secondRaw), remember = false)).map { replaced =>
              readProvider(coordinator).map { provider =>
                assert(
                  replaced == Result.succeed(
                    GitHubConnectionStatus.Connected(grace.value, ConnectionPersistence.Session)
                  )
                )
                assert(provider == Result.succeed(second))
              }
            }
          }
        case _ => assert(false)
      }
    }

    "serializes overlapping remembered connections and commits one coherent final state" in
      Latch.init(1).map { firstEntered =>
        Latch.init(1).map { releaseFirst =>
          Latch.init(1).map { contentionObserved =>
            Latch.init(1).map { secondCompleted =>
              TransitionLock.init(contentionObserved.release).map { transitionLock =>
                val controlled = verifier { candidate =>
                  if candidate == first then firstEntered.release.andThen(releaseFirst.await).andThen(ada)
                  else if candidate == second then grace
                  else Abort.fail(GitHubException.Unauthorized("test rejection"))
                }
                val vault = MemoryVault()

                run(GitHubConnectionCoordinator.init(controlled, Present(vault), transitionLock)).map {
                  case Result.Success(coordinator) =>
                    Fiber.initUnscoped(
                      run(coordinator.connect(TokenSubmission.from(firstRaw), remember = true))
                    ).map { firstFiber =>
                      firstEntered.await.map { _ =>
                        Fiber.initUnscoped(
                          run(coordinator.connect(TokenSubmission.from(secondRaw), remember = true)).map { result =>
                            secondCompleted.release.andThen(result)
                          }
                        ).map { secondFiber =>
                          awaitContentionOrCompletion(contentionObserved, secondCompleted).map { race =>
                            releaseFirst.release.map { _ =>
                              firstFiber.get.map { firstResult =>
                                secondFiber.get.map { secondResult =>
                                  run(coordinator.status()).map { status =>
                                    readProvider(coordinator).map { provider =>
                                      val expected = GitHubConnectionStatus.Connected(
                                        grace.value,
                                        ConnectionPersistence.Device
                                      )
                                      assert(race == TransitionRace.ContentionObserved)
                                      assert(firstResult.isSuccess)
                                      assert(secondResult == Result.succeed(expected))
                                      assert(status == Result.succeed(expected))
                                      assert(provider == Result.succeed(second))
                                      assert(vault.snapshot == Present(secret(secondRaw)))
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  case _ => assert(false)
                }
              }
            }
          }
        }
      }
  }

  "GitHubConnectionCoordinator disconnect" - {
    "disconnect removes an unread stored credential" in {
      val vault = MemoryVault(Present(secret(firstRaw)))
      vault.rejectGet(SecretException.LookupFailed("offline"))

      run(GitHubConnectionCoordinator.init(accepts(first -> ada), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.disconnect()).map { disconnected =>
            assert(disconnected == Result.succeed(()))
            assert(vault.snapshot.isEmpty)
          }
        case _ => assert(false)
      }
    }

    "removes a remembered credential before clearing the connection" in {
      val vault = MemoryVault()
      run(GitHubConnectionCoordinator.init(accepts(first -> ada), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(firstRaw), remember = true)).map { _ =>
            run(coordinator.disconnect()).map { disconnected =>
              readProvider(coordinator).map { provider =>
                assert(disconnected == Result.succeed(()))
                assert(vault.snapshot.isEmpty)
                assert(provider.isFailure)
              }
            }
          }
        case _ => assert(false)
      }
    }

    "finishes clearing a remembered connection when interrupted after removal" in
      Latch.init(1).map { credentialRemoved =>
        Latch.init(1).map { releaseVault =>
          val vault = MemoryVault()
          run(GitHubConnectionCoordinator.init(accepts(first -> ada), Present(vault))).map {
            case Result.Success(coordinator) =>
              run(coordinator.connect(TokenSubmission.from(firstRaw), remember = true)).map { _ =>
                vault.afterNextRemove(credentialRemoved.release.andThen(releaseVault.await))
                Sync.ensure(releaseVault.release) {
                  useTargetFiber(run(coordinator.disconnect())) { disconnectFiber =>
                    awaitWithin(credentialRemoved.await).map { entered =>
                      assert(entered.isSuccess)
                      disconnectFiber.interrupt.map { _ =>
                        releaseVault.release.map { _ =>
                          awaitWithin(disconnectFiber.getResult).map { interrupted =>
                            assert(
                              interrupted match
                                case Result.Panic(_: Interrupted) => true
                                case _                            => false
                            )
                            awaitWithin(awaitStatus(coordinator, GitHubConnectionStatus.Disconnected)).map {
                              completed =>
                                run(coordinator.status()).map { status =>
                                  readProvider(coordinator).map { provider =>
                                    assert(completed.isSuccess)
                                    assert(vault.snapshot.isEmpty)
                                    assert(status == Result.succeed(GitHubConnectionStatus.Disconnected))
                                    assert(provider.isFailure)
                                  }
                                }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            case _ => assert(false)
          }
        }
      }

    "does not remove a vault entry for a session connection" in {
      val vault = MemoryVault()
      run(GitHubConnectionCoordinator.init(accepts(first -> ada), Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.connect(TokenSubmission.from(firstRaw), remember = false)).map { _ =>
            vault.seed(secret(secondRaw))
            run(coordinator.disconnect()).map { disconnected =>
              assert(disconnected == Result.succeed(()))
              assert(vault.snapshot == Present(secret(secondRaw)))
            }
          }
        case _ => assert(false)
      }
    }

    "removes a rejected stored credential" in {
      val vault   = MemoryVault(Present(secret(firstRaw)))
      val rejects = verifier(_ => Abort.fail(GitHubException.Unauthorized("stored credential rejected")))

      run(GitHubConnectionCoordinator.init(rejects, Present(vault))).map {
        case Result.Success(coordinator) =>
          run(coordinator.disconnect()).map { disconnected =>
            run(coordinator.status()).map { status =>
              assert(disconnected == Result.succeed(()))
              assert(status == Result.succeed(GitHubConnectionStatus.Disconnected))
              assert(vault.snapshot.isEmpty)
            }
          }
        case _ => assert(false)
      }
    }

    "serializes an overlapping connection before disconnecting it" in
      Latch.init(1).map { connectEntered =>
        Latch.init(1).map { releaseConnect =>
          Latch.init(1).map { contentionObserved =>
            Latch.init(1).map { disconnectCompleted =>
              TransitionLock.init(contentionObserved.release).map { transitionLock =>
                val controlled = verifier { candidate =>
                  if candidate == first then ada
                  else if candidate == second then
                    connectEntered.release.andThen(releaseConnect.await).andThen(grace)
                  else Abort.fail(GitHubException.Unauthorized("test rejection"))
                }
                val vault = MemoryVault()

                run(GitHubConnectionCoordinator.init(controlled, Present(vault), transitionLock)).map {
                  case Result.Success(coordinator) =>
                    run(coordinator.connect(TokenSubmission.from(firstRaw), remember = true)).map { _ =>
                      Fiber.initUnscoped(
                        run(coordinator.connect(TokenSubmission.from(secondRaw), remember = true))
                      ).map { connectFiber =>
                        connectEntered.await.map { _ =>
                          Fiber.initUnscoped(
                            run(coordinator.disconnect()).map { result =>
                              disconnectCompleted.release.andThen(result)
                            }
                          ).map { disconnectFiber =>
                            awaitContentionOrCompletion(contentionObserved, disconnectCompleted).map { race =>
                              releaseConnect.release.map { _ =>
                                connectFiber.get.map { connectResult =>
                                  disconnectFiber.get.map { disconnectResult =>
                                    run(coordinator.status()).map { status =>
                                      readProvider(coordinator).map { provider =>
                                        assert(race == TransitionRace.ContentionObserved)
                                        assert(connectResult.isSuccess)
                                        assert(disconnectResult == Result.succeed(()))
                                        assert(status == Result.succeed(GitHubConnectionStatus.Disconnected))
                                        assert(provider.isFailure)
                                        assert(vault.snapshot.isEmpty)
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  case _ => assert(false)
                }
              }
            }
          }
        }
      }
  }

  private final class MemoryVault(initial: Maybe[Secret] = Absent) extends SecretVault:
    private var stored: Maybe[Secret]                 = initial
    private var getFailure: Maybe[SecretException]    = Absent
    private var putFailure: Maybe[SecretException]    = Absent
    private var removeFailure: Maybe[SecretException] = Absent
    private var putPanic: Maybe[Throwable]            = Absent
    private var afterPut: Unit < Async                = Kyo.unit
    private var afterRemove: Unit < Async             = Kyo.unit

    def snapshot: Maybe[Secret] = synchronized(stored)

    def seed(value: Secret): Unit = synchronized {
      stored = Present(value)
    }

    def rejectGet(error: SecretException): Unit = synchronized {
      getFailure = Present(error)
    }

    def allowGet(): Unit = synchronized {
      getFailure = Absent
    }

    def rejectPut(error: SecretException): Unit = synchronized {
      putFailure = Present(error)
    }

    def rejectRemove(error: SecretException): Unit = synchronized {
      removeFailure = Present(error)
    }

    def panicPut(error: Throwable): Unit = synchronized {
      putPanic = Present(error)
    }

    def afterNextPut(effect: Unit < Async): Unit = synchronized {
      afterPut = effect
    }

    def afterNextRemove(effect: Unit < Async): Unit = synchronized {
      afterRemove = effect
    }

    def get(service: String, account: String): Maybe[Secret] < (Abort[SecretException] & Async) =
      if !expectedKey(service, account) then Abort.fail(SecretException.LookupFailed("unexpected test key"))
      else
        synchronized(getFailure) match
          case Present(error) => Abort.fail(error)
          case Absent         => synchronized(stored)

    def put(service: String, account: String, secret: Secret): Unit < (Abort[SecretException] & Async) =
      if !expectedKey(service, account) then Abort.fail(SecretException.MutationFailed("unexpected test key"))
      else
        synchronized(putPanic) match
          case Present(throwable) => Sync.defer(throw throwable)
          case Absent             =>
            synchronized(putFailure) match
              case Present(error) => Abort.fail(error)
              case Absent         =>
                Sync.defer {
                  synchronized {
                    stored = Present(secret)
                    val hook = afterPut
                    afterPut = Kyo.unit
                    hook
                  }
                }.map(identity)

    def remove(service: String, account: String): Unit < (Abort[SecretException] & Async) =
      if !expectedKey(service, account) then Abort.fail(SecretException.MutationFailed("unexpected test key"))
      else
        synchronized(removeFailure) match
          case Present(error) => Abort.fail(error)
          case Absent         =>
            Sync.defer {
              synchronized {
                stored = Absent
                val hook = afterRemove
                afterRemove = Kyo.unit
                hook
              }
            }.map(identity)

    private def expectedKey(service: String, account: String): Boolean =
      service == "org.finos.morphir" && account == "github.com"
end GitHubConnectionCoordinatorTests
