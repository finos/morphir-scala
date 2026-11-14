package morphir.spikes.wasmhost

import caseapp.*
import caseapp.core.app.CommandsEntryPoint
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import kyo.*

final case class ProbeOptions(
    @HelpMessage("Wasm guest on disk, never bundled in the host") guest: String = "",
    @HelpMessage("Expected answer export") expectedAnswer: Int = 42,
    @HelpMessage("Measured repeated calls per session") requests: Int = 1,
    @HelpMessage("Warm-up calls, retained in the phase log") warmup: Int = 0,
    @HelpMessage("Open/work/close session cycles") cycles: Int = 1,
    @HelpMessage("Quiet interval at observable lifecycle phases") idleMs: Int = 0,
    @HelpMessage("Bound on emitted response bytes") outputLimit: Int = 65536,
    @HelpMessage("Optional JSON result file") result: String = "",
    @HelpMessage("Assert this executable's runtime name") runtime: String = "",
    @HelpMessage("Run the pinned legacy Morphir compile workload") mep: Boolean = false,
    @HelpMessage("Morphir fixture kind: sdk or python") guestKind: String = "python",
    @HelpMessage("FFM instruction budget per call; other backends do not meter instructions") fuel: Long = 5000000000L
)

private[wasmhost] abstract class HostCli(backend: Backend) extends CommandsEntryPoint:
  override def progName = s"wasm-host-${backend.name}"
  override def commands = Seq(Probe, Supervise, Mep, Safety)

  object Probe extends KyoCommand[ProbeOptions]:
    override def names = List(List("probe"))
    run { options => Sync.defer { ProbeRun.execute(backend, options) } }

  object Supervise extends KyoCommand[SuperviseOptions]:
    override def names = List(List("supervise"))
    run { options =>
      Supervisor.run(HostCli.this.getClass.getName.stripSuffix("$"), backend.name, options)
    }

  object Mep extends KyoCommand[MepOptions]:
    override def names = List(List("mep"))
    run { options => MepWorker.run(backend, options) }

  object Safety extends KyoCommand[SafetyOptions]:
    override def names = List(List("safety"))
    run { options => Sync.defer { SafetyChecks.run(backend, options) } }

private[wasmhost] object ProbeRun:
  private val input = "Morphir: λ / 日本語 / 🚀".getBytes(UTF_8) ++ Array[Byte](0, -1)

  def phase(name: String, cycle: Int, guestMemoryBytes: Maybe[Long] = Absent): Unit =
    val runtime    = Runtime.getRuntime()
    val collectors = java.lang.management.ManagementFactory.getGarbageCollectorMXBeans()
    val gc         = ujson.Arr()
    collectors.forEach { collector =>
      val count  = collector.getCollectionCount()
      val millis = collector.getCollectionTime()
      gc.value += ujson.Obj(
        "name"          -> collector.getName(),
        "collections"   -> (if count < 0 then ujson.Null else ujson.Num(count.toDouble)),
        "elapsedMillis" -> (if millis < 0 then ujson.Null else ujson.Num(millis.toDouble))
      )
    }
    System.err.println(ujson.Obj(
      "phase"                  -> name,
      "cycle"                  -> cycle,
      "pid"                    -> ProcessHandle.current().pid(),
      "nanoTime"               -> System.nanoTime(),
      "heapUsedBytes"          -> ujson.Num((runtime.totalMemory() - runtime.freeMemory()).toDouble),
      "heapCommittedBytes"     -> ujson.Num(runtime.totalMemory().toDouble),
      "heapMaxBytes"           -> ujson.Num(runtime.maxMemory().toDouble),
      "gc"                     -> gc,
      "guestLinearMemoryBytes" -> guestMemoryBytes.fold[ujson.Value](ujson.Null)(bytes => ujson.Num(bytes.toDouble)),
      "heapCoverage"           -> "runtime heap only, excludes libextism allocations"
    ).render())

  def execute(backend: Backend, options: ProbeOptions): Unit =
    require(options.guest.nonEmpty, "--guest is required")
    require(options.runtime.isEmpty || options.runtime == backend.name, "runtime does not match executable")
    require(options.requests >= 1 && options.warmup >= 0 && options.cycles >= 1, "invalid call/cycle count")
    require(options.idleMs >= 0 && options.idleMs <= 10000, "idle interval must be 0..10000 ms")
    require(options.outputLimit >= 1 && options.outputLimit <= GuestFiles.MaxBytes, "invalid output bound")
    val guest         = Path.of(options.guest).toAbsolutePath
    val hash          = GuestFiles.sha256(GuestFiles.read(guest))
    val start         = System.nanoTime()
    val callNanos     = scala.collection.mutable.ArrayBuffer.empty[Long]
    def quiet(): Unit = if options.idleMs > 0 then Thread.sleep(options.idleMs.toLong)
    phase("parent-ready", 0)
    quiet()
    @scala.annotation.tailrec
    def calls(work: () => Unit, remaining: Int): Unit =
      if remaining > 0 then
        val began = System.nanoTime()
        work()
        callNanos += (System.nanoTime() - began)
        calls(work, remaining - 1)
    @scala.annotation.tailrec
    def sessions(cycle: Int): Unit =
      if cycle <= options.cycles then
        phase("load-start", cycle)
        val session = backend.openWithFuel(guest, options.outputLimit, options.fuel)
        try
          val protocol: Maybe[MepWorkload] =
            if options.mep then Present(MepWorkload(session, options.guestKind)) else Absent
          if !options.mep then
            require(session.answer == options.expectedAnswer, "answer export differs from expected value")
          def work(): Unit = protocol match
            case Present(workload) => workload.compile()
            case Absent            =>
              val reply = session.handle(input)
              require(
                reply.status == input.length && java.util.Arrays.equals(reply.bytes, input),
                "byte round-trip failed"
              )
          work()
          phase("first-result", cycle)
          phase("loaded-idle", cycle, session.guestMemoryBytes)
          quiet()
          phase("warmup", cycle)
          calls(() => work(), options.warmup)
          phase("calls", cycle)
          calls(() => work(), options.requests)
          phase("calls-complete", cycle, session.guestMemoryBytes)
          quiet()
          protocol match
            case Present(workload) => workload.finish()
            case Absent            => ()
        finally session.close()
        phase("closed", cycle)
        quiet()
        sessions(cycle + 1)
    sessions(1)
    val result = ujson.Obj(
      "scope"        -> (if options.mep then "legacy-mep-compile-workload" else "synthetic-byte-callback-probe"),
      "runtime"      -> backend.name,
      "guest"        -> guest.toString,
      "guestSha256"  -> hash,
      "answer"       -> options.expectedAnswer,
      "requests"     -> options.requests,
      "warmup"       -> options.warmup,
      "cycles"       -> options.cycles,
      "elapsedNanos" -> (System.nanoTime() - start),
      "callNanosIncludingWarmup" -> ujson.Arr.from(callNanos.map(value => ujson.Num(value.toDouble))),
      "instructionMetering"      -> (if backend.name == "extism-ffm" then "libextism fuel" else "unavailable"),
      "requestedFuel"            -> options.fuel,
      "javaVersion"              -> System.getProperty("java.version"),
      "os"                       -> System.getProperty("os.name"),
      "arch"                     -> System.getProperty("os.arch"),
      "qualified"                -> true
    )
    if options.result.nonEmpty then
      val path = Path.of(options.result).toAbsolutePath
      Files.createDirectories(path.getParent)
      val _ = Files.writeString(path, result.render(indent = 2))
    println(result.render())
