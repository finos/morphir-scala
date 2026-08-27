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
final case class ServerOptions(
    @HelpMessage("Port to bind on 127.0.0.1. Use 0 to select an available port.")
    port: Int = 0,
    @HelpMessage("Do not open the browser after the server starts.")
    noOpen: Boolean = false
)

object ServerOptions:
  private given boundedPortParser: ArgParser[Int] =
    ArgParser.int.xmapError(
      identity,
      port =>
        if port >= 0 && port <= 65535 then Right(port)
        else Left(Error.Other("port must be between 0 and 65535"))
    )

  given parser: Parser[ServerOptions] = Parser.derive[ServerOptions]

object ServerCommand extends Command[ServerOptions]:
  private val WebHostClassName        = "morphir.web.server.WebHost$"
  private val NativeImageCodeProperty = "org.graalvm.nativeimage.imagecode"

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

  override def name = "server"

  def run(options: ServerOptions, remainingArgs: RemainingArgs): Unit =
    ServerRunner.run(options, remainingArgs)

  private object ServerRunner extends KyoCommand[ServerOptions]:
    override def name = "server"

    this.run { options => ServerCommand.run(options, ServerCommandLive.dependencies) }

  private[cli] def available: Boolean =
    nativeImageRuntime(sys.props.toMap) || webHostAvailable(getClass.getClassLoader)

  private[cli] def nativeImageRuntime(properties: Map[String, String]): Boolean =
    properties.get(NativeImageCodeProperty).contains("runtime")

  private[cli] def webHostAvailable(classLoader: ClassLoader): Boolean =
    try
      Class.forName(WebHostClassName, false, classLoader)
      true
    catch case _: ClassNotFoundException => false

  private[cli] def run(
      options: ServerOptions,
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

  private def validate(options: ServerOptions)(using Frame): Unit < Abort[Throwable] =
    if options.port >= 0 && options.port <= 65535 then Kyo.unit
    else Abort.fail(IllegalArgumentException("port must be between 0 and 65535"))
