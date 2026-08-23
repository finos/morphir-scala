package org.finos.morphir.cli

import kyo.*
import morphir.appkit.SecretVault
import morphir.connector.github.GitHubTokenVerifier
import morphir.web.server.WebHost

private[cli] object ServeCommandLive:

  def dependencies: ServeCommand.Dependencies =
    ServeCommand.Dependencies(
      LiveHost,
      ServeCommand.DesktopBrowserLauncher(ServeCommand.DesktopPlatform.system, ServeCommand.Output.console),
      ServeCommand.Output.console
    )

  private object LiveHost extends ServeCommand.Host:
    def start(
        config: ServeCommand.HostConfig,
        browserLauncher: ServeCommand.BrowserLauncher
    )(using Frame): ServeCommand.BoundHost < (Async & Scope & Abort[Throwable]) =
      val started: AnyRef < (Async & Scope & Abort[Throwable]) = WebHost
        .startWithLauncher(
          WebHost.Config(port = config.port, openBrowser = config.openBrowser),
          url => browserLauncher.open(url),
          GitHubTokenVerifier.live,
          Present(SecretVault.system)
        )
      started.map { value =>
        val port = value.asInstanceOf[WebHost.BoundHost].port
        LiveBoundHost(port, () => value.asInstanceOf[WebHost.BoundHost].await)
      }

  private final class LiveBoundHost(
      val port: Int,
      waitForClose: () => Unit < Async
  ) extends ServeCommand.BoundHost:
    def await(using Frame): Unit < Async = waitForClose()
