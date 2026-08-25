package millbuild

import java.nio.file.{Files, LinkOption, Path}
import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption
import java.util.concurrent.{ConcurrentHashMap, Executors, ScheduledExecutorService, ThreadFactory, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

object DesktopSmoke {
  private val sentinel                 = "ghp_MORPHIR_TASK6_SENTINEL_TOKEN_1234567890"
  private[millbuild] val scanChunkSize = 8192

  val launchEnvironment: Map[String, String] = Map("MORPHIR_DESKTOP_SMOKE_SENTINEL" -> sentinel)

  def childEnvironment(inherited: Map[String, String]): Map[String, String] =
    inherited.removed("ELECTRON_RUN_AS_NODE") ++ launchEnvironment

  def platformToken(osName: String, architecture: String): String = {
    val os = osName.toLowerCase(java.util.Locale.ROOT) match
      case value if value.contains("mac") || value.contains("darwin") => "darwin"
      case value if value.contains("win")                             => "windows"
      case value if value.contains("linux")                           => "linux"
      case value => value.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")
    val arch = architecture.toLowerCase(java.util.Locale.ROOT) match
      case "aarch64" | "arm64" => "arm64"
      case "amd64" | "x86_64"  => "x64"
      case value               => value.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")
    s"$os-$arch"
  }

  def withoutSourceMapTrailer(source: String): String =
    source.replaceAll("(?m)^//# sourceMappingURL=main\\.js\\.map\\r?\\n?", "")

  final case class Result(assertions: Map[String, Boolean])

  object Result {
    given upickle.default.ReadWriter[Result] =
      upickle.default.readwriter[Map[String, Boolean]].bimap[Result](_.assertions, Result(_))
  }

  final case class Artifacts(
      screenshot: os.Path,
      result: os.Path,
      processLog: os.Path,
      rendererLog: os.Path
  )

  final case class SafeRunRoot private[DesktopSmoke] (path: os.Path)

  enum BoundaryKind {
    case DarwinProcessGroup, LinuxSession, WindowsTaskkillBestEffort
  }

  final case class ProcessBoundary(
      kind: BoundaryKind,
      launchCommand: Seq[String],
      marker: Option[os.Path],
      completion: Option[os.Path],
      generatedSource: Option[GeneratedSource] = None
  )

  final case class GeneratedSource(path: os.Path, content: String)

  final case class DiagnosticTail(text: String, truncated: Boolean)

  enum ProcessStatus {
    case Completed, TimedOut, Interrupted, LaunchFailed
  }

  final case class ProcessResult(
      status: ProcessStatus,
      exitCode: Option[Int],
      treeStopped: Boolean,
      detail: String
  )

  def npmCommand(platform: String): Seq[String] =
    if platform.startsWith("windows-") then
      Seq("cmd.exe", "/d", "/s", "/c", "npm ci --ignore-scripts")
    else Seq("npm", "ci", "--ignore-scripts")

  private val windowsBoundarySource =
    """import java.nio.charset.StandardCharsets;
      |import java.nio.file.Files;
      |import java.nio.file.Path;
      |import java.util.Arrays;
      |import java.util.concurrent.CountDownLatch;
      |
      |final class WindowsProcessBoundary {
      |  public static void main(String[] args) throws Exception {
      |    if (args.length < 3) throw new IllegalArgumentException("boundary command is missing");
      |    Path marker = Path.of(args[0]);
      |    Path completion = Path.of(args[1]);
      |    var command = Arrays.asList(Arrays.copyOfRange(args, 2, args.length));
      |    Process child = new ProcessBuilder(command).inheritIO().start();
      |    Files.writeString(marker, Long.toString(child.pid()), StandardCharsets.UTF_8);
      |    int exitCode = child.waitFor();
      |    Files.writeString(completion, Integer.toString(exitCode), StandardCharsets.UTF_8);
      |    new CountDownLatch(1).await();
      |    System.exit(exitCode);
      |  }
      |}
      |""".stripMargin

  private def javaExecutable(platform: String): String = {
    val name = if platform.startsWith("windows-") then "java.exe" else "java"
    java.nio.file.Paths.get(System.getProperty("java.home"), "bin", name).toAbsolutePath.normalize().toString
  }

  private[millbuild] def processBoundary(
      platform: String,
      command: Seq[String],
      stateDirectory: os.Path,
      setsidExecutable: Option[String]
  ): Either[String, ProcessBoundary] = {
    val marker         = stateDirectory / "group-id"
    val completion     = stateDirectory / "completion"
    val markerPath     = marker.toNIO.getParent.toRealPath().resolve(marker.last).toString
    val completionPath = completion.toNIO.getParent.toRealPath().resolve(completion.last).toString
    if platform.startsWith("darwin-") then {
      val script =
        "set -m\n" +
          "\"$@\" &\n" +
          "target=$!\n" +
          "group=$(/bin/ps -o pgid= -p \"$target\" | /usr/bin/tr -d ' ')\n" +
          "if [ \"$group\" != \"$target\" ]; then kill \"$target\" 2>/dev/null; wait \"$target\"; exit 125; fi\n" +
          "printf '%s\\n' \"$group\" > \"$0\"\n" +
          "wait \"$target\"\n"
      Right(ProcessBoundary(
        BoundaryKind.DarwinProcessGroup,
        Seq("/bin/sh", "-c", script, markerPath) ++ command,
        Some(marker),
        None
      ))
    } else if platform.startsWith("linux-") then {
      val script =
        "group=$(/bin/ps -o pgid= -p \"$$\" | /usr/bin/tr -d ' ')\n" +
          "if [ \"$group\" != \"$$\" ]; then exit 125; fi\n" +
          "printf '%s\\n' \"$group\" > \"$0\"\n" +
          "exec \"$@\"\n"
      setsidExecutable
        .toRight("desktop smoke requires setsid on Linux")
        .map(path =>
          ProcessBoundary(
            BoundaryKind.LinuxSession,
            Seq(path, "--wait", "/bin/sh", "-c", script, markerPath) ++ command,
            Some(marker),
            None
          )
        )
    } else if platform.startsWith("windows-") then {
      val source     = stateDirectory / "WindowsProcessBoundary.java"
      val sourcePath = source.toNIO.getParent.toRealPath().resolve(source.last).toString
      Right(
        ProcessBoundary(
          BoundaryKind.WindowsTaskkillBestEffort,
          Seq(javaExecutable(platform), sourcePath, markerPath, completionPath) ++ command,
          Some(marker),
          Some(completion),
          Some(GeneratedSource(source, windowsBoundarySource))
        )
      )
    } else Left(s"unsupported desktop smoke process platform: $platform")
  }

  val expectedAssertions: Set[String] = Set(
    "clearedAfterFailure",
    "clearedAfterSessionSuccess",
    "clearedAfterSuccess",
    "disconnectedThroughButton",
    "mountedRenderer",
    "rememberFalseReadLive",
    "rememberReadLive",
    "rememberTrueReadLive",
    "removedStoredCredentialThroughButton",
    "rendererConsoleSentinelFree",
    "retainedOnFailure",
    "retainedOnSessionSuccess",
    "retainedOnSuccess",
    "safeConnectedStatus",
    "safeRejectedError",
    "safeSessionStatus",
    "submittedThroughForm",
    "transientDomSentinelFree"
  )

  def validate(result: Result): Either[String, Unit] = {
    val actual  = result.assertions.keySet
    val missing = (expectedAssertions -- actual).toSeq.sorted
    val extra   = (actual -- expectedAssertions).toSeq.sorted

    if missing.nonEmpty || extra.nonEmpty then
      Left(
        s"desktop smoke assertion keys differ: missing [${missing.mkString(", ")}]; extra [${extra.mkString(", ")}]"
      )
    else
      result.assertions.iterator.collect { case (name, false) => name }.toSeq.sorted.headOption
        .toLeft(())
        .left
        .map(name => s"desktop smoke assertion failed: $name")
  }

  def redact(value: String, sentinel: String): String =
    if sentinel.isEmpty then value else value.replace(sentinel, "<redacted>")

  def redact(value: String): String = redact(value, sentinel)

  private def failure(message: String): Left[String, Nothing] = Left(redact(message))

  def artifacts(runRoot: SafeRunRoot): Artifacts = {
    val root = runRoot.path / "artifacts"
    Artifacts(
      screenshot = root / "screenshot.png",
      result = root / "result.json",
      processLog = root / "process.log",
      rendererLog = root / "renderer.log"
    )
  }

  def decodeResult(json: String): Either[String, Result] =
    try {
      val result = upickle.default.read[Result](json)
      validate(result).map(_ => result)
    } catch
      case NonFatal(error) => failure(s"desktop smoke result did not decode: ${error.getMessage}")

  def verifyProcessAndArtifacts(exitCode: Int, artifacts: Artifacts): Either[String, Result] =
    try
      if exitCode != 0 then failure(s"desktop smoke process exited with exit $exitCode")
      else
        val required = Seq(artifacts.screenshot, artifacts.result, artifacts.processLog, artifacts.rendererLog)
        required.find(path => !os.isFile(path)) match
          case Some(path) => failure(s"desktop smoke required artifact is missing: ${path.last}")
          case None       =>
            val payloads = Seq(artifacts.screenshot, artifacts.result)
            payloads.find(path => !Files.isRegularFile(path.toNIO, LinkOption.NOFOLLOW_LINKS)) match
              case Some(path) => failure(s"desktop smoke required artifact is not a regular file: ${path.last}")
              case None       =>
                payloads.find(path => Files.size(path.toNIO) == 0L) match
                  case Some(path) => failure(s"desktop smoke required artifact is empty: ${path.last}")
                  case None       => decodeResult(os.read(artifacts.result))
    catch
      case NonFatal(error) => failure(s"desktop smoke artifact verification failed: ${error.getClass.getSimpleName}")

  def scanForSentinel(artifacts: Artifacts, userData: os.Path): Either[String, Unit] =
    try {
      val artifactFiles = Seq(artifacts.screenshot, artifacts.result, artifacts.processLog, artifacts.rendererLog)
      val userDataFiles =
        if os.isDir(userData) then os.walk(userData).filter(os.isFile(_))
        else Seq.empty

      (artifactFiles.filter(os.isFile(_)) ++ userDataFiles).find(containsSentinel) match
        case Some(path) => failure(s"desktop smoke sentinel $sentinel leaked into ${path.toString}")
        case None       => Right(())
    } catch {
      case NonFatal(error) => failure(s"desktop smoke sentinel scan failed: ${error.getClass.getSimpleName}")
    }

  private def containsSentinel(path: os.Path): Boolean = {
    val needle = sentinel.getBytes(StandardCharsets.UTF_8)
    Using.resource(Files.newInputStream(path.toNIO)) { input =>
      val buffer = new Array[Byte](scanChunkSize)

      @annotation.tailrec
      def loop(overlap: Array[Byte]): Boolean = {
        val count = input.read(buffer)
        if count < 0 then false
        else if count == 0 then loop(overlap)
        else {
          val window = new Array[Byte](overlap.length + count)
          System.arraycopy(overlap, 0, window, 0, overlap.length)
          System.arraycopy(buffer, 0, window, overlap.length, count)
          if containsBytes(window, needle) then true
          else {
            val retained = math.min(needle.length - 1, window.length)
            loop(window.takeRight(retained))
          }
        }
      }

      loop(Array.emptyByteArray)
    }
  }

  private def containsBytes(bytes: Array[Byte], needle: Array[Byte]): Boolean = {
    @annotation.tailrec
    def matchesAt(offset: Int, index: Int): Boolean =
      if index == needle.length then true
      else if bytes(offset + index) != needle(index) then false
      else matchesAt(offset, index + 1)

    @annotation.tailrec
    def loop(offset: Int): Boolean =
      if offset + needle.length > bytes.length then false
      else if matchesAt(offset, 0) then true
      else loop(offset + 1)

    needle.isEmpty || loop(0)
  }

  def mergeLogs(sources: Seq[os.Path], target: os.Path): Unit =
    Using.resource(Files.newOutputStream(target.toNIO)) { output =>
      val buffer = new Array[Byte](scanChunkSize)
      sources.filter(os.isFile(_)).foreach { source =>
        Using.resource(Files.newInputStream(source.toNIO)) { input =>
          @annotation.tailrec
          def loop(): Unit = {
            val count = input.read(buffer)
            if count < 0 then ()
            else if count == 0 then loop()
            else {
              output.write(buffer, 0, count)
              loop()
            }
          }
          loop()
        }
      }
    }

  private def readTail(path: os.Path, length: Int): Array[Byte] =
    if length <= 0 then Array.emptyByteArray
    else
      Using.resource(FileChannel.open(path.toNIO, StandardOpenOption.READ)) { channel =>
        val size   = channel.size()
        val actual = math.min(length.toLong, size).toInt
        val buffer = ByteBuffer.allocate(actual)
        channel.position(size - actual)

        @annotation.tailrec
        def loop(): Unit =
          if !buffer.hasRemaining then ()
          else if channel.read(buffer) < 0 then ()
          else loop()

        loop()
        buffer.array()
      }

  def cappedDiagnosticTail(sources: Seq[os.Path], maxBytes: Int = 65536): DiagnosticTail = {
    require(maxBytes > 0, "diagnostic tail limit must be positive")
    val existing = sources.filter(os.isFile(_))
    val total    = existing.iterator.map(os.size).sum

    @annotation.tailrec
    def select(remainingSources: List[os.Path], remaining: Int, selected: List[(os.Path, Int)]): List[(os.Path, Int)] =
      remainingSources match
        case _ if remaining <= 0 => selected
        case Nil                 => selected
        case path :: tail        =>
          val count = math.min(os.size(path), remaining.toLong).toInt
          select(tail, remaining - count, (path -> count) :: selected)

    val selected = select(existing.reverse.toList, maxBytes, Nil)
    val output   = new java.io.ByteArrayOutputStream(maxBytes)
    selected.foreach { case (path, count) => output.write(readTail(path, count)) }
    val truncated = total > maxBytes
    val prefix    = if truncated then "[truncated]\n" else ""
    DiagnosticTail(prefix + redact(String(output.toByteArray, StandardCharsets.UTF_8)), truncated)
  }

  private val processObservationMillis = 10L
  private val processCleanupMillis     = 5000L

  private def descendants(handle: ProcessHandle): Seq[ProcessHandle] =
    try {
      val stream = handle.descendants()
      try stream.iterator().asScala.toSeq
      finally stream.close()
    } catch {
      case NonFatal(_) => Seq.empty
    }

  private def expandRetained(root: ProcessHandle, retained: java.util.Set[ProcessHandle]): Seq[ProcessHandle] = {
    val known      = root +: retained.asScala.toSeq
    val discovered = known.flatMap(descendants)
    discovered.foreach(retained.add)
    (root +: retained.asScala.toSeq).groupBy(_.pid()).valuesIterator.map(_.head).toSeq
  }

  private final class ProcessObserver(root: ProcessHandle) {
    private val retained                           = ConcurrentHashMap.newKeySet[ProcessHandle]()
    private val stopping                           = new AtomicBoolean(false)
    private val executor: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
      override def newThread(runnable: Runnable): Thread = {
        val thread = new Thread(runnable, "desktop-smoke-process-observer")
        thread.setDaemon(true)
        thread
      }
    })
    private val scheduled = executor.scheduleWithFixedDelay(
      () => if !stopping.get() then expandRetained(root, retained),
      0L,
      processObservationMillis,
      TimeUnit.MILLISECONDS
    )
    private var stopResult: Option[Boolean] = None

    def handles: java.util.Set[ProcessHandle] = retained

    def stop(): Boolean = synchronized {
      stopResult match
        case Some(result) => result
        case None         =>
          stopping.set(true)
          scheduled.cancel(true)
          executor.shutdownNow()
          var interrupted = false
          val deadline    = System.nanoTime() + TimeUnit.SECONDS.toNanos(1L)

          @annotation.tailrec
          def loop(): Boolean = {
            val remaining = deadline - System.nanoTime()
            if remaining <= 0L then executor.isTerminated
            else
              try executor.awaitTermination(remaining, TimeUnit.NANOSECONDS)
              catch {
                case _: InterruptedException =>
                  interrupted = true
                  loop()
              }
          }

          val terminated = loop()
          if terminated then expandRetained(root, retained)
          if interrupted then Thread.currentThread().interrupt()
          stopResult = Some(terminated)
          terminated
    }
  }

  private def stopRetainedTree(root: ProcessHandle, retained: java.util.Set[ProcessHandle]): Boolean = {
    val started       = System.nanoTime()
    val graceDeadline = started + TimeUnit.SECONDS.toNanos(1L)
    val forceDeadline = started + TimeUnit.MILLISECONDS.toNanos(processCleanupMillis)
    var interrupted   = false

    def live(): Seq[ProcessHandle] = expandRetained(root, retained).filter(isHandleRunning)

    def pause(): Unit =
      try Thread.sleep(20L)
      catch {
        case _: InterruptedException => interrupted = true
      }

    @annotation.tailrec
    def graceful(): Seq[ProcessHandle] = {
      val targets = live()
      targets.foreach(_.destroy())
      if targets.isEmpty || System.nanoTime() >= graceDeadline then targets
      else {
        pause()
        graceful()
      }
    }

    @annotation.tailrec
    def force(): Seq[ProcessHandle] = {
      val targets = live()
      targets.foreach(_.destroyForcibly())
      if targets.isEmpty || System.nanoTime() >= forceDeadline then targets
      else {
        pause()
        force()
      }
    }

    try {
      val afterGrace = graceful()
      if afterGrace.isEmpty then true else force().isEmpty
    } finally if interrupted then Thread.currentThread().interrupt()
  }

  private final case class ActiveBoundary(kind: BoundaryKind, id: Long, completion: Option[os.Path])

  private def resolveExecutable(name: String): Option[String] =
    Option(System.getenv("PATH")).toSeq
      .flatMap(_.split(java.io.File.pathSeparator).toSeq)
      .map(directory => java.nio.file.Paths.get(directory).resolve(name))
      .find(path => Files.isRegularFile(path) && Files.isExecutable(path))
      .map(_.toAbsolutePath.normalize().toString)

  private def utilityExit(command: Seq[String], timeoutMillis: Long = 1000L): Option[Int] =
    try {
      val process = new ProcessBuilder(command.asJava)
        .redirectOutput(ProcessBuilder.Redirect.DISCARD)
        .redirectError(ProcessBuilder.Redirect.DISCARD)
        .start()
      try
        if process.waitFor(timeoutMillis, TimeUnit.MILLISECONDS) then Some(process.exitValue())
        else {
          process.destroyForcibly()
          process.waitFor(timeoutMillis, TimeUnit.MILLISECONDS)
          None
        }
      finally {
        if process.isAlive then process.destroyForcibly()
        process.getInputStream.close()
        process.getErrorStream.close()
        process.getOutputStream.close()
      }
    } catch {
      case NonFatal(_) => None
    }

  private final class BoundedDrainer(input: java.io.InputStream, maxBytes: Int, processId: Long) {
    private val output    = new java.io.ByteArrayOutputStream(Math.min(maxBytes, 4096))
    private val overflow  = new AtomicBoolean(false)
    private val failed    = new AtomicBoolean(false)
    private val completed = new AtomicBoolean(false)
    val thread            = new Thread(
      () => {
        val buffer = new Array[Byte](4096)
        try {
          @annotation.tailrec
          def loop(total: Int): Unit = {
            val count = input.read(buffer)
            if count >= 0 then {
              val retained = Math.min(count, maxBytes - total)
              if retained > 0 then output.write(buffer, 0, retained)
              if retained < count then overflow.set(true)
              loop(total + retained)
            }
          }

          loop(0)
        } catch {
          case NonFatal(_) => failed.set(true)
        } finally completed.set(true)
      },
      s"morphir-desktop-output-$processId"
    )
    thread.setDaemon(true)

    def result: Option[String] =
      if completed.get() && !failed.get() && !overflow.get() then Some(output.toString(StandardCharsets.UTF_8))
      else None
  }

  private[millbuild] def boundedCommandOutput(
      command: Seq[String],
      timeoutMillis: Long,
      maxBytes: Long
  ): Option[String] = {
    require(command.nonEmpty, "bounded utility command must not be empty")
    require(timeoutMillis > 0L, "bounded utility timeout must be positive")
    require(maxBytes > 0L && maxBytes <= Int.MaxValue, "bounded utility output limit must fit a byte array")
    var process: Process        = null
    var drainer: BoundedDrainer = null
    var interrupted             = false
    val cleanupTimeoutMillis    = Math.min(Math.max(timeoutMillis, 100L), 1000L)

    def awaitProcess(deadline: Long): Boolean = {
      @annotation.tailrec
      def loop(): Boolean =
        if !process.isAlive then true
        else {
          val remaining = deadline - System.nanoTime()
          if remaining <= 0L then false
          else {
            val result = try Some(process.waitFor(remaining, TimeUnit.NANOSECONDS))
            catch {
              case _: InterruptedException =>
                interrupted = true
                None
            }
            result match
              case Some(exited) => exited
              case None         => loop()
          }
        }
      loop()
    }

    def awaitDrainer(deadline: Long): Boolean = {
      @annotation.tailrec
      def loop(): Boolean =
        if !drainer.thread.isAlive then true
        else {
          val remaining = deadline - System.nanoTime()
          if remaining <= 0L then false
          else {
            try {
              val waitMillis = Math.max(1L, TimeUnit.NANOSECONDS.toMillis(remaining))
              drainer.thread.join(waitMillis)
            } catch {
              case _: InterruptedException =>
                interrupted = true
            }
            loop()
          }
        }
      loop()
    }

    def cleanup(action: => Unit): Unit =
      try action
      catch {
        case _: InterruptedException => interrupted = true
        case NonFatal(_)             => ()
      }

    try {
      process = new ProcessBuilder(command.asJava)
        .redirectError(ProcessBuilder.Redirect.DISCARD)
        .start()
      drainer = new BoundedDrainer(process.getInputStream, maxBytes.toInt, process.pid())
      drainer.thread.start()
      val exited =
        try process.waitFor(timeoutMillis, TimeUnit.MILLISECONDS)
        catch {
          case _: InterruptedException =>
            interrupted = true
            false
        }
      if !exited then {
        process.destroyForcibly()
        awaitProcess(System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(cleanupTimeoutMillis))
      }
      val drained = awaitDrainer(System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(cleanupTimeoutMillis))
      if exited && drained && !interrupted && process.exitValue() == 0 then drainer.result else None
    } catch {
      case NonFatal(_) => None
    } finally {
      if process != null then {
        cleanup(if process.isAlive then process.destroyForcibly())
        cleanup(awaitProcess(System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(cleanupTimeoutMillis)))
        cleanup(process.getInputStream.close())
        cleanup(process.getErrorStream.close())
        cleanup(process.getOutputStream.close())
      }
      if drainer != null && drainer.thread.isAlive then {
        cleanup(drainer.thread.interrupt())
        cleanup(awaitDrainer(System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(cleanupTimeoutMillis)))
      }
      if interrupted then Thread.currentThread().interrupt()
    }
  }

  private def hostPlatform: String =
    platformToken(System.getProperty("os.name", ""), System.getProperty("os.arch", ""))

  private def recognizedZombieState(state: String, platform: String): Boolean =
    if platform.startsWith("linux-") then state.matches("Z[<LNsl+]*")
    else state.matches("Z[<>AELNSVWXsl+]*")

  private def processStateRunning(state: String, threadCount: Option[Int], platform: String): Boolean =
    if !recognizedZombieState(state, platform) then true
    else if platform.startsWith("linux-") then threadCount.forall(_ > 1)
    else false

  private def processRowRunning(line: String, platform: String): Option[Boolean] = {
    val fields = line.trim.split("\\s+").toList
    val parsed =
      if platform.startsWith("linux-") then
        fields match
          case state :: Nil if state.nonEmpty            => Some(processStateRunning(state, None, platform))
          case state :: threads :: Nil if state.nonEmpty =>
            val threadCount = threads.toIntOption.filter(_ > 0)
            Some(processStateRunning(state, threadCount, platform))
          case _ => None
      else
        fields match
          case state :: Nil if state.nonEmpty => Some(processStateRunning(state, None, platform))
          case _                              => None
    parsed
  }

  private[millbuild] def processOutputRunning(output: String, platform: String): Option[Boolean] = {
    val rows = output.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    if rows.isEmpty then None
    else {
      val parsed = rows.map(processRowRunning(_, platform))
      if parsed.exists(_.isEmpty) then None else Some(parsed.flatten.exists(identity))
    }
  }

  private[millbuild] def groupOutputRunning(output: String, platform: String, groupId: Long): Option[Boolean] = {
    val rows = output.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    if rows.isEmpty then None
    else {
      val parsed = rows.map { line =>
        line.split("\\s+", 2).toList match
          case group :: processState :: Nil =>
            group.toLongOption.map { value =>
              if value == groupId then processRowRunning(processState, platform) else Some(false)
            }
          case group :: Nil => group.toLongOption.map(value => if value == groupId then None else Some(false))
          case _            => None
      }
      if parsed.exists(_.isEmpty) || parsed.flatten.exists(_.isEmpty) then None
      else Some(parsed.flatten.flatten.exists(identity))
    }
  }

  private def processState(pid: Long): Option[Boolean] = {
    val platform = hostPlatform
    val columns  = if platform.startsWith("linux-") then "stat=,nlwp=" else "stat="
    boundedCommandOutput(Seq("/bin/ps", "-o", columns, "-p", pid.toString), 1000L, 1024L)
      .flatMap(processOutputRunning(_, platform))
  }

  private def isHandleRunning(handle: ProcessHandle): Boolean =
    if !handle.isAlive then false
    else if !Files.isExecutable(java.nio.file.Path.of("/bin/ps")) then true
    else processState(handle.pid()).getOrElse(true)

  private[millbuild] def isProcessRunning(pid: Long): Boolean =
    ProcessHandle.of(pid).toScala.exists(isHandleRunning)

  private[millbuild] def unixGroupSignalCommand(signal: String, groupId: Long): Seq[String] =
    Seq("/bin/kill", s"-$signal", "--", s"-$groupId")

  private def processGroup(pid: Long): Option[Long] =
    try {
      val process = new ProcessBuilder("/bin/ps", "-o", "pgid=", "-p", pid.toString)
        .redirectError(ProcessBuilder.Redirect.DISCARD)
        .start()
      try {
        val exited = process.waitFor(1000L, TimeUnit.MILLISECONDS)
        if !exited || process.exitValue() != 0 then None
        else {
          val reader = new java.io.BufferedReader(
            new java.io.InputStreamReader(process.getInputStream, StandardCharsets.UTF_8)
          )
          try Option(reader.readLine()).flatMap(_.trim.toLongOption)
          finally reader.close()
        }
      } finally {
        if process.isAlive then process.destroyForcibly()
        process.getInputStream.close()
        process.getErrorStream.close()
        process.getOutputStream.close()
      }
    } catch {
      case NonFatal(_) => None
    }

  private def readLong(path: os.Path): Option[Long] =
    if !os.isFile(path) || os.size(path) > 64L then None
    else
      Using.resource(Files.newBufferedReader(path.toNIO, StandardCharsets.UTF_8)) { reader =>
        Option(reader.readLine()).flatMap(_.trim.toLongOption)
      }

  private def awaitBoundary(
      boundary: ProcessBoundary,
      process: Process,
      deadline: Long
  ): Option[ActiveBoundary] = {
    val ownGroup =
      if boundary.kind == BoundaryKind.WindowsTaskkillBestEffort then None
      else processGroup(ProcessHandle.current().pid())

    @annotation.tailrec
    def loop(): Option[ActiveBoundary] = {
      val candidate = boundary.kind match
        case BoundaryKind.DarwinProcessGroup        => boundary.marker.flatMap(readLong)
        case BoundaryKind.LinuxSession              => boundary.marker.flatMap(readLong)
        case BoundaryKind.WindowsTaskkillBestEffort =>
          boundary.marker.flatMap(readLong).filter(_ > 1L).map(_ => process.pid())
      candidate match
        case Some(id) if id > 1L && !ownGroup.contains(id) =>
          Some(ActiveBoundary(boundary.kind, id, boundary.completion))
        case _ if System.nanoTime() >= deadline || !process.isAlive => None
        case _                                                      =>
          Thread.sleep(processObservationMillis)
          loop()
    }

    try loop()
    catch {
      case _: InterruptedException =>
        Thread.currentThread().interrupt()
        None
    }
  }

  private def unixGroupHasRunningProcess(groupId: Long): Option[Boolean] =
    val platform = hostPlatform
    val columns  = if platform.startsWith("linux-") then "pgid=,stat=,nlwp=" else "pgid=,stat="
    boundedCommandOutput(Seq("/bin/ps", "-axo", columns), 1000L, 1024L * 1024L)
      .flatMap(groupOutputRunning(_, platform, groupId))

  private def unixGroupAlive(groupId: Long): Boolean =
    utilityExit(unixGroupSignalCommand("0", groupId)).contains(0) &&
      unixGroupHasRunningProcess(groupId).getOrElse(true)

  private[millbuild] def stopUnixGroup(groupId: Long): Boolean = {
    val started       = System.nanoTime()
    val graceDeadline = started + TimeUnit.SECONDS.toNanos(1L)
    val forceDeadline = started + TimeUnit.MILLISECONDS.toNanos(processCleanupMillis)

    utilityExit(unixGroupSignalCommand("TERM", groupId))

    @annotation.tailrec
    def graceful(): Boolean =
      if !unixGroupAlive(groupId) then true
      else if System.nanoTime() >= graceDeadline then false
      else {
        Thread.sleep(20L)
        graceful()
      }

    @annotation.tailrec
    def force(): Boolean =
      if !unixGroupAlive(groupId) then true
      else if System.nanoTime() >= forceDeadline then false
      else {
        utilityExit(unixGroupSignalCommand("KILL", groupId))
        Thread.sleep(20L)
        force()
      }

    try if graceful() then true else force()
    catch {
      case _: InterruptedException =>
        Thread.currentThread().interrupt()
        false
    }
  }

  private def stopWindowsTree(rootPid: Long): Boolean = {
    val killed   = utilityExit(Seq("taskkill", "/PID", rootPid.toString, "/T", "/F"), 10000L).contains(0)
    val deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(processCleanupMillis)

    @annotation.tailrec
    def loop(): Boolean =
      ProcessHandle.of(rootPid).toScala match
        case None                               => true
        case Some(handle) if !handle.isAlive    => true
        case _ if System.nanoTime() >= deadline => false
        case _                                  =>
          Thread.sleep(20L)
          loop()

    try killed && loop()
    catch {
      case _: InterruptedException =>
        Thread.currentThread().interrupt()
        false
    }
  }

  private def stopBoundary(boundary: ActiveBoundary): Boolean = boundary.kind match
    case BoundaryKind.DarwinProcessGroup | BoundaryKind.LinuxSession => stopUnixGroup(boundary.id)
    case BoundaryKind.WindowsTaskkillBestEffort                      => stopWindowsTree(boundary.id)

  def runProcess(
      command: Seq[String],
      workingDirectory: os.Path,
      environment: Map[String, String],
      environmentRemovals: Set[String],
      timeout: FiniteDuration,
      stdout: os.Path,
      stderr: os.Path,
      platform: String
  ): ProcessResult = {
    require(command.nonEmpty, "desktop smoke process command must not be empty")
    require(timeout > Duration.Zero, "desktop smoke process timeout must be positive")
    val stateDirectory                         = stdout / os.up / s".${stdout.last}.boundary"
    var process: Process                       = null
    var observer: ProcessObserver              = null
    var activeBoundary: Option[ActiveBoundary] = None
    var interrupted                            = false

    def finish(status: ProcessStatus, exitCode: Option[Int], detail: String): ProcessResult = {
      val boundaryStopped = activeBoundary.exists(stopBoundary)
      val observerStopped = observer == null || observer.stop()
      val retained        = if observer == null then ConcurrentHashMap.newKeySet[ProcessHandle]() else observer.handles
      val handlesStopped  = process == null || stopRetainedTree(process.toHandle, retained)
      val treeStopped     = process == null || (boundaryStopped && observerStopped && handlesStopped)
      if treeStopped then os.remove.all(stateDirectory)
      val shutdownDetail =
        if treeStopped then detail
        else
          s"$detail; shutdown incomplete (boundary=$boundaryStopped, observer=$observerStopped, handles=$handlesStopped)"
      ProcessResult(status, exitCode, treeStopped, redact(shutdownDetail))
    }

    os.remove.all(stateDirectory)
    os.makeDir.all(stateDirectory)
    val boundaryPlan =
      if platform.startsWith("linux-") then
        resolveExecutable("setsid") match
          case None => Left("desktop smoke requires setsid on Linux")
          case Some(path) if !utilityExit(Seq(path, "--wait", "/bin/true"), 2000L).contains(0) =>
            Left("desktop smoke requires setsid --wait support on Linux")
          case Some(path) => processBoundary(platform, command, stateDirectory, Some(path))
      else processBoundary(platform, command, stateDirectory, None)
    boundaryPlan match
      case Left(message) =>
        os.remove.all(stateDirectory)
        ProcessResult(ProcessStatus.LaunchFailed, None, treeStopped = true, redact(message))
      case Right(boundary) =>
        try {
          boundary.generatedSource.foreach(source => os.write.over(source.path, source.content))
          val started  = System.nanoTime()
          val deadline = started + timeout.toNanos
          val builder  = new ProcessBuilder(boundary.launchCommand.asJava)
            .directory(workingDirectory.toNIO.toRealPath().toFile)
            .redirectOutput(
              ProcessBuilder.Redirect.to(stdout.toNIO.getParent.toRealPath().resolve(stdout.last).toFile)
            )
            .redirectError(
              ProcessBuilder.Redirect.to(stderr.toNIO.getParent.toRealPath().resolve(stderr.last).toFile)
            )
          val childEnvironment = builder.environment()
          val configured       = (childEnvironment.asScala.toMap -- environmentRemovals) ++ environment
          childEnvironment.clear()
          childEnvironment.putAll(configured.asJava)
          process = builder.start()
          observer = new ProcessObserver(process.toHandle)
          val activationDeadline = math.min(deadline, System.nanoTime() + TimeUnit.SECONDS.toNanos(5L))
          activeBoundary = awaitBoundary(boundary, process, activationDeadline)
          activeBoundary match
            case None => finish(ProcessStatus.LaunchFailed, None, "process ownership boundary did not activate")
            case Some(active) if active.kind == BoundaryKind.WindowsTaskkillBestEffort =>
              @annotation.tailrec
              def awaitCompletion(): Option[Long] =
                active.completion.flatMap(readLong) match
                  case result @ Some(_)                      => result
                  case None if System.nanoTime() >= deadline => None
                  case None if !process.isAlive              => None
                  case None                                  =>
                    Thread.sleep(processObservationMillis)
                    awaitCompletion()

              val completed = awaitCompletion()
              completed match
                case Some(exitCode) => finish(ProcessStatus.Completed, Some(exitCode.toInt), "completed")
                case None if System.nanoTime() >= deadline => finish(ProcessStatus.TimedOut, None, "timed out")
                case None => finish(ProcessStatus.LaunchFailed, None, "Windows boundary exited before completion")
            case Some(_) =>
              val remaining = deadline - System.nanoTime()
              val exited    = remaining > 0L && process.waitFor(remaining, TimeUnit.NANOSECONDS)
              if exited then finish(ProcessStatus.Completed, Some(process.exitValue()), "completed")
              else finish(ProcessStatus.TimedOut, None, "timed out")
        } catch {
          case _: InterruptedException =>
            interrupted = true
            finish(ProcessStatus.Interrupted, None, "interrupted")
          case NonFatal(error) =>
            finish(ProcessStatus.LaunchFailed, None, s"launch failed: ${error.getClass.getSimpleName}")
        } finally {
          if observer != null then observer.stop()
          if process != null then {
            process.getInputStream.close()
            process.getErrorStream.close()
            process.getOutputStream.close()
          }
          if interrupted then Thread.currentThread().interrupt()
        }
  }

  def cleanup(runRoot: SafeRunRoot): Unit = os.remove.all(runRoot.path)

  def safeRunRoot(base: os.Path, candidate: os.Path): Either[String, SafeRunRoot] = {
    val lexicalBase      = base.toNIO.toAbsolutePath.normalize()
    val lexicalCandidate = candidate.toNIO.toAbsolutePath.normalize()

    if lexicalCandidate == lexicalBase then
      Left("desktop smoke run root must be a strict descendant of its base")
    else if !lexicalCandidate.startsWith(lexicalBase) then
      Left("desktop smoke run root must be lexically contained by its base")
    else if Files.isSymbolicLink(lexicalBase) then
      Left("desktop smoke base must not be a symbolic link")
    else if !Files.isDirectory(lexicalBase, LinkOption.NOFOLLOW_LINKS) then
      Left("desktop smoke base must be an existing directory")
    else
      validateComponents(lexicalBase, lexicalCandidate).flatMap { _ =>
        try {
          val physicalBase      = lexicalBase.toRealPath()
          val physicalCandidate = lexicalCandidate.toRealPath()
          validatePhysicalContainment(physicalBase, physicalCandidate).map(SafeRunRoot(_))
        } catch {
          case NonFatal(error) =>
            Left(s"cannot resolve desktop smoke run root: ${error.getClass.getSimpleName}")
        }
      }
  }

  private[millbuild] def validatePhysicalContainment(base: Path, candidate: Path): Either[String, os.Path] =
    if candidate == base || !candidate.startsWith(base) then
      Left("desktop smoke run root must be physically contained by its base")
    else Right(os.Path(candidate))

  private def validateComponents(base: Path, candidate: Path): Either[String, Unit] = {
    val components = base.relativize(candidate).iterator().asScala.toSeq
    val paths      = components.scanLeft(base)(_.resolve(_)).tail

    if paths.exists(Files.isSymbolicLink(_)) then
      Left("desktop smoke run root must not traverse a symbolic link")
    else if !Files.isDirectory(candidate, LinkOption.NOFOLLOW_LINKS) then
      Left("desktop smoke run root must be an existing directory")
    else Right(())
  }
}
