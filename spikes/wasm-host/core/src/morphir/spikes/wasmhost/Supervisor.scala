package morphir.spikes.wasmhost

import caseapp.*
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import kyo.*
import scala.jdk.CollectionConverters.*

final case class SuperviseOptions(
    guest: String = "",
    expectedAnswer: Int = 42,
    requests: Int = 100,
    warmup: Int = 10,
    cycles: Int = 1,
    concurrency: Int = 1,
    idleMs: Int = 250,
    quietMs: Int = 2000,
    timeoutMs: Int = 30000,
    cancelAfterMs: Int = 0,
    result: String = "",
    mep: Boolean = false,
    guestKind: String = "python"
)

/** The parent never executes guest code. A native downcall cannot prevent worker termination. */
private[wasmhost] object Supervisor:
  private def command(mainClass: String): Seq[String] =
    if System.getProperty("org.graalvm.nativeimage.imagecode") == "runtime" then
      Seq(ProcessHandle.current().info().command().orElseThrow())
    else
      Seq(
        Path.of(System.getProperty("java.home"), "bin", "java").toString,
        "--enable-native-access=ALL-UNNAMED",
        "-cp",
        System.getProperty("java.class.path"),
        mainClass
      )

  private final class Worker(mainClass: String, options: SuperviseOptions) extends AutoCloseable:
    val output           = Files.createTempFile("wasm-worker-", ".json")
    private val launched = System.nanoTime()
    private val overflow = new java.util.concurrent.atomic.AtomicBoolean(false)
    val process          = new ProcessBuilder((command(mainClass) ++ Seq(
      "probe",
      "--guest",
      options.guest,
      "--expected-answer",
      options.expectedAnswer.toString,
      "--requests",
      options.requests.toString,
      "--warmup",
      options.warmup.toString,
      "--idle-ms",
      options.idleMs.toString
    ) ++
      (if options.mep then Seq("--mep", "--guest-kind", options.guestKind, "--output-limit", "1048576")
       else Seq.empty)).asJava)
      .start()
    private def drain(input: java.io.InputStream, limit: Int, sink: java.io.OutputStream, name: String): Thread =
      Thread.ofVirtual().name(name).start(() =>
        try
          val buffer = new Array[Byte](4096)
          @scala.annotation.tailrec
          def loop(total: Int): Unit =
            val n = input.read(buffer)
            if n >= 0 then
              if total.toLong + n > limit then
                overflow.set(true)
                val _ = process.destroyForcibly()
              else
                sink.write(buffer, 0, n)
                sink.flush()
                loop(total + n)
          loop(0)
        catch case _: java.io.IOException => ()
      )
    private val stdout              = Files.newOutputStream(output)
    private val outputPump          = drain(process.getInputStream, 1048576, stdout, "wasm-worker-output")
    private val errorPump           = drain(process.getErrorStream, 65536, System.err, "wasm-worker-diagnostics")
    def reaped: Boolean             = !process.isAlive && !outputPump.isAlive && !errorPump.isAlive
    def close(): Unit               = this.synchronized { closeWorker() }
    private def closeWorker(): Unit =
      try
        if process.isAlive then
          process.destroy()
          if !process.waitFor(200, TimeUnit.MILLISECONDS) then
            process.destroyForcibly()
            require(process.waitFor(5000, TimeUnit.MILLISECONDS), "worker survived forced termination")
        require(!process.isAlive, "worker remains active")
        outputPump.join(2000)
        errorPump.join(2000)
        require(!outputPump.isAlive && !errorPump.isAlive, "worker pipe thread remains active")
      finally
        process.getInputStream.close()
        process.getErrorStream.close()
        process.getOutputStream.close()
        stdout.close()
        val _ = Files.deleteIfExists(output)

    def await(options: SuperviseOptions): ujson.Obj =
      val bound     = options.timeoutMs
      val remaining = math.max(0L, TimeUnit.MILLISECONDS.toNanos(bound.toLong) - (System.nanoTime() - launched))
      val completed = process.waitFor(remaining, TimeUnit.NANOSECONDS)
      if completed then outputPump.join(2000)
      val status = if overflow.get() then "output-bound"
      else if completed then
        if process.exitValue() == 0 then "pass" else "guest-failed"
      else "deadline"
      val bytes = Files.size(output)
      require(bytes <= 1024 * 1024, "worker result exceeds 1 MiB")
      ujson.Obj(
        "pid"    -> process.pid(),
        "status" -> status,
        "exit"   -> (if completed then ujson.Num(process.exitValue()) else ujson.Null),
        "result" -> (if completed && process.exitValue() == 0 then ujson.read(Files.readString(output)) else ujson.Null)
      )

    def awaitAsync(options: SuperviseOptions): ujson.Obj < Async =
      val deadline                  = launched + TimeUnit.MILLISECONDS.toNanos(options.timeoutMs.toLong)
      def loop(): ujson.Obj < Async = Async.defer {
        if process.isAlive && System.nanoTime() < deadline then Async.sleep(25.millis).andThen(loop())
        else Sync.defer(await(options))
      }
      loop()

  def run(mainClass: String, runtime: String, options: SuperviseOptions): Unit < (Async & Sync) =
    require(options.guest.nonEmpty && Files.isRegularFile(Path.of(options.guest)), "--guest must name a file")
    require(
      options.cycles > 0 && options.cycles <= 100 && options.concurrency >= 1 && options.concurrency <= 2,
      "invalid cycle/concurrency count"
    )
    require(
      options.timeoutMs > 0 && options.cancelAfterMs >= 0 && options.quietMs >= 0,
      "invalid deadline/quiet interval"
    )
    require(
      options.requests > 0 && options.warmup >= 0 && options.idleMs >= 0 && options.idleMs <= 10000,
      "invalid workload options"
    )
    val receipts                                = scala.collection.mutable.ArrayBuffer.empty[ujson.Obj]
    val live                                    = scala.collection.mutable.ArrayBuffer.empty[Worker]
    def loop(cycle: Int): Unit < (Async & Sync) =
      if cycle > options.cycles then
        Sync.defer {
          val result = ujson.Obj(
            "scope"     -> "synthetic-supervised-probe",
            "runtime"   -> runtime,
            "workers"   -> ujson.Arr.from(receipts),
            "allReaped" -> true
          )
          if options.result.nonEmpty then
            val path = Path.of(options.result).toAbsolutePath
            Files.createDirectories(path.getParent)
            val _ = Files.writeString(path, result.render(indent = 2))
          println(result.render())
          val statuses = receipts.map(_("status").str)
          val code     = if statuses.contains("deadline") then 4
          else if statuses.contains("cancelled") then 5
          else if statuses.exists(_ != "pass") then 3 else 0
          if code != 0 then System.exit(code)
        }
      else
        Scope.run {
          Kyo.foreach(1 to options.concurrency)(_ =>
            Scope.acquire(Sync.defer {
              val worker = new Worker(mainClass, options)
              live += worker
              worker
            })
          ).map { workers =>
            Sync.defer { ProbeRun.phase("workers-started", cycle) }.andThen(
              Kyo.foreach(workers)(worker =>
                worker.awaitAsync(options).map { receipt =>
                  val _ = receipts += receipt
                }
              ).map(_ => ())
            )
          }
        }.andThen(Sync.defer {
          require(live.forall(_.reaped), "worker or pipe thread survived scoped teardown")
          live.clear()
          ProbeRun.phase("workers-reaped", cycle)
          Thread.sleep(options.quietMs.toLong)
          ProbeRun.phase("parent-cleanup-idle", cycle)
        }).andThen(loop(cycle + 1))
    val operation = Sync.defer { ProbeRun.phase("parent-ready", 0) }.andThen(loop(1))
    if options.cancelAfterMs == 0 then operation
    else
      Abort.run[Timeout](Async.timeout(options.cancelAfterMs.millis)(operation)).map { result =>
        Sync.defer {
          result match
            case Result.Success(_)   => ()
            case Result.Panic(error) => throw error
            case Result.Failure(_)   =>
              live.foreach(_.close())
              require(live.forall(_.reaped), "Kyo interruption left a worker or pipe thread active")
              val cancelled = ujson.Obj(
                "scope"     -> "kyo-interruption-proof",
                "runtime"   -> runtime,
                "allReaped" -> true,
                "workers"   -> ujson.Arr.from(live.map(worker =>
                  ujson.Obj("pid" -> worker.process.pid(), "status" -> "cancelled")
                ))
              )
              if options.result.nonEmpty then
                val path = Path.of(options.result).toAbsolutePath
                Files.createDirectories(path.getParent)
                val _ = Files.writeString(path, cancelled.render(indent = 2))
              println(cancelled.render())
              System.exit(5)
        }
      }
