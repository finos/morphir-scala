package org.finos.morphir.cli

import caseapp.*
import caseapp.core.Error
import caseapp.core.RemainingArgs
import caseapp.core.argparser.ArgParser
import caseapp.core.app.Command
import caseapp.core.parser.Parser
import java.awt.Desktop
import java.net.URI
import kyo.*
import kyo.kernel.Effect

@AppName("Serve the Morphir browser application on loopback.")
final case class ServeOptions(
    @HelpMessage("Port to bind on 127.0.0.1. Use 0 to select an available port.")
    port: Int = 0,
    @HelpMessage("Do not open the browser after the server starts.")
    noOpen: Boolean = false
)

object ServeOptions:
  private given boundedPortParser: ArgParser[Int] =
    ArgParser.int.xmapError(
      identity,
      port =>
        if port >= 0 && port <= 65535 then Right(port)
        else Left(Error.Other("port must be between 0 and 65535"))
    )

  given parser: Parser[ServeOptions] = Parser.derive[ServeOptions]

object ServeCommand extends Command[ServeOptions]:
  private val WebHostClassName = "morphir.web.server.WebHost$"

  private[cli] val browserWarning = "Warning: Unable to open the browser."

  private[cli] final case class HostConfig(port: Int, openBrowser: Boolean)

  private[cli] trait BrowserLauncher:
    def open(url: String)(using Frame): Boolean < Async

  private[cli] trait BoundHost:
    def port: Int
    def await(using Frame): Unit < Async

  private[cli] trait Host:
    def start(
        config: HostConfig,
        browserLauncher: BrowserLauncher
    )(using Frame): BoundHost < (Async & Scope & Abort[Throwable])

  private[cli] trait Output:
    def listening(port: Int)(using Frame): Unit < Sync
    def browserLaunchFailed(using Frame): Unit < Sync

  private[cli] object Output:
    val console: Output = new Output:
      def listening(port: Int)(using Frame): Unit < Sync =
        Console.printLine(s"Listening on http://127.0.0.1:$port")

      def browserLaunchFailed(using Frame): Unit < Sync =
        Console.printLineErr(browserWarning)

  private[cli] trait DesktopPlatform:
    def browse(url: String): Unit

  private[cli] object DesktopPlatform:
    val system: DesktopPlatform = new DesktopPlatform:
      def browse(url: String): Unit =
        if !Desktop.isDesktopSupported then throw UnsupportedOperationException("Desktop browsing is unavailable")
        val desktop = Desktop.getDesktop
        if !desktop.isSupported(Desktop.Action.BROWSE) then
          throw UnsupportedOperationException("Desktop browsing is unavailable")
        desktop.browse(URI(url))

  private[cli] final class DesktopBrowserLauncher(
      desktop: DesktopPlatform,
      output: Output
  ) extends BrowserLauncher:
    def open(url: String)(using Frame): Boolean < Async =
      Effect.catching(Async.defer(desktop.browse(url)).andThen(true))((_: Throwable) => false).map {
        case true  => true
        case false => output.browserLaunchFailed.andThen(false)
      }

  private[cli] final case class Dependencies(
      host: Host,
      browserLauncher: BrowserLauncher,
      output: Output
  )

  override def name = "serve"

  def run(options: ServeOptions, remainingArgs: RemainingArgs): Unit =
    ServeRunner.run(options, remainingArgs)

  private object ServeRunner extends KyoCommand[ServeOptions]:
    override def name = "serve"

    this.run { options => ServeCommand.run(options, ServeCommandLive.dependencies) }

  private[cli] def available: Boolean =
    webHostAvailable(getClass.getClassLoader)

  private[cli] def webHostAvailable(classLoader: ClassLoader): Boolean =
    try
      Class.forName(WebHostClassName, false, classLoader)
      true
    catch case _: ClassNotFoundException => false

  private[cli] def run(
      options: ServeOptions,
      dependencies: Dependencies
  )(using Frame): Unit < (Async & Abort[Throwable]) =
    validate(options).andThen {
      Scope.run {
        dependencies.host
          .start(
            HostConfig(port = options.port, openBrowser = !options.noOpen),
            dependencies.browserLauncher
          )
          .map { host =>
            dependencies.output.listening(host.port).andThen(host.await)
          }
      }
    }

  private def validate(options: ServeOptions)(using Frame): Unit < Abort[Throwable] =
    if options.port >= 0 && options.port <= 65535 then Kyo.unit
    else Abort.fail(IllegalArgumentException("port must be between 0 and 65535"))
