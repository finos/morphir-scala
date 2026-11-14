package morphir.spikes.wasmhost.polyglot

import caseapp.core.app.CommandsEntryPoint
import java.io.{ByteArrayOutputStream, RandomAccessFile}
import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets.UTF_8
import org.graalvm.polyglot.{Context, HostAccess, PolyglotAccess, Source, Value}
import org.graalvm.polyglot.io.{ByteSequence, IOAccess}
import org.graalvm.polyglot.proxy.{ProxyArray, ProxyExecutable, ProxyObject}
import kyo.*

final case class CompilerOptions(
    bundle: String = "",
    workspace: String = "",
    arguments: String = "",
    output: String = "",
    mode: String = "compiler",
    uniqueImports: Boolean = false,
    verifyWasm: String = "",
    expectedResult: Int = 42,
    holdMs: Int = 0
)

object Main extends CommandsEntryPoint:
  override def progName = "wasm-host-polyglot-compiler"
  override def commands = Seq(Run)
  object Run extends KyoCommand[CompilerOptions]:
    override def names = List(List("run"))
    run(options => Sync.defer(CompilerProof.run(options)))

private[polyglot] object CompilerProof:
  private def sha(bytes: Array[Byte]): String =
    java.security.MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 255}%02x").mkString

  // Import names may change; all other sections must remain byte-for-byte identical.
  private def sections(bytes: Array[Byte]): Vector[(Int, String)] =
    def unsigned(offset: Int): (Int, Int) =
      @annotation.tailrec
      def loop(index: Int, shift: Int, value: Int): (Int, Int) =
        require(index < bytes.length && shift < 35, "invalid section size")
        val next   = bytes(index) & 255
        val result = value | ((next & 127) << shift)
        if (next & 128) == 0 then (result, index + 1) else loop(index + 1, shift + 7, result)
      loop(offset, 0, 0)
    @annotation.tailrec
    def loop(offset: Int, result: Vector[(Int, String)]): Vector[(Int, String)] =
      if offset == bytes.length then result
      else
        val id             = bytes(offset) & 255
        val (length, body) = unsigned(offset + 1)
        require(length >= 0 && body.toLong + length <= bytes.length, "invalid section extent")
        loop(body + length, result :+ (id -> sha(bytes.slice(body, body + length))))
    require(bytes.take(8).sameElements(Array[Byte](0, 97, 115, 109, 1, 0, 0, 0)), "invalid Wasm header")
    loop(8, Vector.empty)

  private final class BoundedOutput extends ByteArrayOutputStream:
    override def write(bytes: Array[Byte], offset: Int, length: Int): Unit =
      require(size().toLong + length <= 1048576, "compiler diagnostics exceed 1 MiB")
      super.write(bytes, offset, length)
    override def write(value: Int): Unit =
      require(size() < 1048576, "compiler diagnostics exceed 1 MiB")
      super.write(value)

  def run(options: CompilerOptions): Unit =
    require(
      options.bundle.nonEmpty && options.workspace.nonEmpty && options.output.nonEmpty,
      "bundle/workspace/output required"
    )
    val bundle    = Path.of(options.bundle).toRealPath()
    val workspace = Path.of(options.workspace).toAbsolutePath.normalize()
    Files.createDirectories(workspace)
    val output = Path.of(options.output).toAbsolutePath.normalize()
    Files.createDirectories(output)
    val generated = if options.verifyWasm.isEmpty then Maybe.empty[Path]
    else
      require(
        !Path.of(options.verifyWasm).isAbsolute && Path.of(options.verifyWasm).getNameCount == 1 &&
          !options.verifyWasm.startsWith("."),
        "expected a workspace file name"
      )
      val target = workspace.resolve(options.verifyWasm)
      Files.deleteIfExists(target)
      Maybe(target)
    val arguments = if options.arguments.isEmpty then Vector("-v")
    else ujson.read(Files.readString(Path.of(options.arguments))).arr.toVector.map(_.str)
    val stdout  = BoundedOutput()
    val stderr  = BoundedOutput()
    val runtime = RuntimeBridge(bundle, workspace.toRealPath(), stdout, stderr)
    val started = System.nanoTime()
    require(options.holdMs >= 0 && options.holdMs <= 1000, "phase hold exceeds 1 second")
    val context = Context.newBuilder("js", "wasm").allowExperimentalOptions(true)
      .option("js.webassembly", "true").option("js.text-encoding", "true")
      .option("js.crypto", "true").option("js.performance", "true")
      .option("wasm.LegacyExceptions", "true")
      .allowPolyglotAccess(PolyglotAccess.newBuilder().allowEvalBetween("js", "wasm")
        .allowBindingsAccess("js").allowBindingsAccess("wasm").build())
      .allowHostAccess(HostAccess.NONE).allowIO(IOAccess.NONE)
      .out(stdout).err(stderr).build()
    val phases                    = scala.collection.mutable.ArrayBuffer.empty[ujson.Value]
    def phase(name: String): Unit =
      val event = ujson.Obj(
        "phase"         -> name,
        "pid"           -> ProcessHandle.current().pid(),
        "nanoTime"      -> System.nanoTime(),
        "elapsedNanos"  -> (System.nanoTime() - started),
        "heapUsedBytes" -> (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())
      )
      phases += event
      System.err.println(event.render())
      if options.holdMs > 0 then Thread.sleep(options.holdMs.toLong)
    var status              = "fail"
    var failure             = ""
    var features            = ""
    val provenance          = ujson.Obj()
    var result: ujson.Value = ujson.Null
    try
      phase("context-ready")
      val bindings = context.getBindings("js")
      bindings.putMember("__bridge", runtime.proxy)
      bindings.putMember("__arguments", ProxyArray.fromArray(arguments.map(_.asInstanceOf[AnyRef])*))
      bindings.putMember("__uniqueImports", options.uniqueImports)
      val input = getClass.getResourceAsStream("/compiler-runtime.js")
      require(input != null, "compiler runtime resource missing")
      val shim = try String(input.readAllBytes(), UTF_8)
      finally input.close()
      provenance("runtimeSha256") = sha(shim.getBytes(UTF_8))
      provenance("loaderSha256") = sha(Files.readAllBytes(bundle.resolve("moonc.js")))
      context.eval("js", shim)
      features = context.eval(
        "js",
        "JSON.stringify({webassembly:typeof WebAssembly,tag:typeof WebAssembly.Tag,jsTag:typeof WebAssembly.JSTag,textEncoder:typeof TextEncoder,textDecoder:typeof TextDecoder,weakRef:typeof WeakRef})"
      ).asString()
      phase("runtime-ready")
      val loader = if options.mode == "bounds" then
        """Promise.resolve().then(()=>{
          let denied=0;
          for(const action of [()=>__bridge.readAll('/etc/passwd'),
            ()=>__bridge.open('/toolchain/moonc.js',577),
            ()=>__bridge.readAll('/workspace/../outside'),
            ()=>__bridge.readAll('/workspace/escape'),
            ()=>require('node:child_process')]){
              try{action();}catch(error){denied++;}
          }
          if(denied!==5)throw Error('workspace authority check failed: '+denied);
          if(new TextDecoder().decode(new TextEncoder().encode('héλ'))!=='héλ')throw Error('UTF-8 roundtrip failed');
          const tag=new WebAssembly.Tag({parameters:['i32']});
          const error=new WebAssembly.Exception(tag,[42]);
          if(!error.is(tag)||error.getArg(tag,0)!==42)throw Error('exception tag roundtrip failed');
          globalThis.__boundsPassed=denied;
        })"""
      else if options.mode == "gc-direct" then
        val module = context.eval(Source.newBuilder(
          "wasm",
          ByteSequence.create(Files.readAllBytes(workspace.resolve("gc-import.wasm"))),
          "gc-import-control"
        ).build())
        val instance = module.newInstance(context.eval("js", "({bindings:{f:()=>3}})"))
        require(
          instance.getMember("exports").getMember("run").execute().asInt() == 3,
          "direct GC import result differs"
        )
        "Promise.resolve(3)"
      else if options.mode == "gc-probe" then
        "WebAssembly.instantiate(new Uint8Array(__bridge.readAll('/workspace/gc-import.wasm')), {bindings:{f:()=>3}}).then(r=>{if(r.instance.exports.run()!==3)throw Error('GC-import result differs');})"
      else if options.mode == "duplicate-arity" then
        "WebAssembly.instantiate(new Uint8Array(__bridge.readAll('/workspace/duplicate-arity.wasm')), {bindings:{f:(...args)=>args.reduce((a,b)=>a+b,0)}}).then(r=>{if(r.instance.exports.run()!==15)throw Error('duplicate-import result differs');})"
      else if options.mode == "duplicate-probe" then
        "WebAssembly.instantiate(new Uint8Array(__bridge.readAll('/workspace/duplicate.wasm')), {bindings:{f:(...args)=>args.reduce((a,b)=>a+b,0)}}).then(r=>{if(r.instance.exports.run()!==15)throw Error('duplicate-import result differs');})"
      else Files.readString(bundle.resolve("moonc.js"))
      val promise  = context.eval(Source.newBuilder("js", loader, "upstream-moonc.js").build())
      var settled  = false
      var rejected = ""
      val _        = promise.invokeMember(
        "then",
        new ProxyExecutable:
          def execute(values: Value*): Object = { settled = true; null }
        ,
        new ProxyExecutable:
          def execute(values: Value*): Object =
            settled = true
            rejected = values.headOption.fold("unknown JavaScript rejection")(_.toString)
            null
      )
      @annotation.tailrec
      def loop(remaining: Int): Unit =
        if !settled && remaining > 0 then
          context.eval("js", "0")
          loop(remaining - 1)
      loop(100)
      phase("compiler-returned")
      require(settled, "compiler promise did not settle")
      val exit = context.eval("js", "globalThis.__compilerExit").asInt()
      require(rejected.isEmpty || rejected.contains("__COMPILER_EXIT__"), rejected)
      require(exit == 0, s"compiler exited $exit")
      if options.uniqueImports then
        val rewritten = bindings.getMember("__rewrittenModule")
        require(rewritten != null && rewritten.hasArrayElements, "adapted module missing")
        val adapted = Array.tabulate(Math.toIntExact(rewritten.getArraySize))(i =>
          rewritten.getArrayElement(i.toLong).asInt().toByte
        )
        if options.mode == "compiler" then
          val stream   = Files.list(bundle.resolve("moonc.assets"))
          val original = try
            val modules = stream.toArray.map(_.asInstanceOf[Path]).filter(_.toString.endsWith(".wasm"))
            require(modules.length == 1, "expected one upstream compiler module")
            Files.readAllBytes(modules.head)
          finally stream.close()
          require(
            sections(original).filter(_._1 != 2) == sections(adapted).filter(_._1 != 2),
            "adapter changed a non-import section"
          )
          provenance("originalModuleSha256") = sha(original)
          provenance("adaptedModuleSha256") = sha(adapted)
          provenance("nonImportSectionsUnchanged") = true
          val _ = Files.write(output.resolve("adapted-module.wasm"), adapted)
        provenance("importAliases") = ujson.read(context.eval("js", "JSON.stringify(__importAliases)").asString())
      generated.foreach { target =>
        require(
          Files.isRegularFile(target) && Files.size(target) <= 64L * 1024 * 1024,
          "compiler output missing or too large"
        )
        val bytes  = Files.readAllBytes(target)
        val module = context.eval(Source.newBuilder("wasm", ByteSequence.create(bytes), "compiled-moonbit").build())
        val answer = module.newInstance().getMember("exports").getMember("answer").execute().asInt()
        require(answer == options.expectedResult, s"generated answer $answer differs from ${options.expectedResult}")
        result = ujson.Obj("answer" -> answer, "bytes" -> bytes.length, "sha256" -> sha(bytes))
        Files.write(output.resolve("generated.wasm"), bytes)
        phase("first-result")
      }
      status = "pass"
    catch
      case error: Throwable =>
        failure = error.toString.take(8192)
        val trace = new java.io.PrintWriter(Files.newBufferedWriter(output.resolve("failure.txt")))
        try error.printStackTrace(trace)
        finally trace.close()
    finally
      context.close(true)
      runtime.close()
      phase("closed")
      Files.writeString(output.resolve("stdout.txt"), stdout.toString(UTF_8))
      Files.writeString(output.resolve("stderr.txt"), stderr.toString(UTF_8))
      val receipt = ujson.Obj(
        "status"        -> status,
        "error"         -> failure,
        "mode"          -> options.mode,
        "uniqueImports" -> options.uniqueImports,
        "holdMs"        -> options.holdMs,
        "arguments"     -> ujson.Arr.from(arguments),
        "graalRelease"  -> "25.5.5.0.3",
        "javaVersion"   -> System.getProperty("java.version"),
        "target" -> (if System.getProperty("org.graalvm.nativeimage.imagecode") == "runtime" then "native" else "jvm"),
        "features"            -> features,
        "provenance"          -> provenance,
        "generated"           -> result,
        "phases"              -> ujson.Arr.from(phases),
        "fileOperations"      -> runtime.operations,
        "openFilesAfterClose" -> runtime.openFiles,
        "loader"              -> bundle.resolve("moonc.js").toString,
        "authority"           ->
          "read-only /toolchain and /core, private writable /workspace; no ambient IO, network or subprocesses"
      )
      Files.writeString(output.resolve("receipt.json"), receipt.render(indent = 2))
      println(receipt.render())
    if status != "pass" then throw IllegalStateException(failure)

private[polyglot] final class RuntimeBridge(
    bundle: Path,
    workspace: Path,
    stdout: java.io.OutputStream,
    stderr: java.io.OutputStream
) extends AutoCloseable:
  private val files  = scala.collection.mutable.Map.empty[Int, RandomAccessFile]
  private var nextFd = 3
  val operations     = ujson.Arr()
  def openFiles: Int = files.size
  private def path(text: String, write: Boolean = false): Path =
    val roots = Vector(
      "/toolchain" -> bundle,
      "/core"      -> bundle.getParent.resolve("core/core").toRealPath(),
      "/workspace" -> workspace
    )
    val (prefix, root) = roots.find((prefix, _) => text == prefix || text.startsWith(prefix + "/")).getOrElse(
      throw IllegalArgumentException(s"virtual path denied: $text")
    )
    require(!write || prefix == "/workspace", "toolchain and core are read-only")
    val target = root.resolve(text.drop(prefix.length).stripPrefix("/")).normalize()
    require(target.startsWith(root), "path escaped virtual root")
    val checked = if Files.exists(target) then target.toRealPath() else target.getParent.toRealPath()
    require(checked.startsWith(root), "symlink escaped virtual root")
    target
  private def array(bytes: Array[Byte]): ProxyArray = new ProxyArray:
    def getSize(): Long                      = bytes.length.toLong
    def get(index: Long): Object             = Integer.valueOf(bytes(Math.toIntExact(index)) & 255)
    def set(index: Long, value: Value): Unit = throw UnsupportedOperationException("read-only byte input")
  private def function(name: String)(body: Seq[Value] => AnyRef): ProxyExecutable = new ProxyExecutable:
    def execute(args: Value*): Object =
      require(operations.arr.size < 10000, "compiler file-operation count exceeds proof bound")
      operations.arr += ujson.Obj(
        "operation" -> name,
        "arguments" -> ujson.Arr.from(args.map(value =>
          if value.isString then value.asString().take(256) else value.toString.take(256)
        ))
      )
      body(args)
  private val methods = new java.util.HashMap[String, Object]()
  methods.put(
    "readAll",
    function("readAll")(args =>
      val file = path(args.head.asString())
      require(Files.size(file) <= 64L * 1024 * 1024, "compiler input file exceeds 64 MiB")
      array(Files.readAllBytes(file))
    )
  )
  methods.put("exists", function("exists")(args => java.lang.Boolean.valueOf(Files.exists(path(args.head.asString())))))
  methods.put(
    "isDir",
    function("isDir")(args => java.lang.Boolean.valueOf(Files.isDirectory(path(args.head.asString()))))
  )
  methods.put(
    "open",
    function("open")(args =>
      require(files.size < 256, "compiler file-descriptor bound")
      val flags    = args(1).asInt()
      val writable = (flags & 3) != 0
      val file     = path(args.head.asString(), writable)
      require(Files.exists(file) || (flags & 64) != 0, "ENOENT")
      val handle = RandomAccessFile(file.toFile, if writable then "rw" else "r")
      if (flags & 512) != 0 then handle.setLength(0)
      if (flags & 1024) != 0 then handle.seek(handle.length())
      val fd = nextFd
      nextFd += 1
      files(fd) = handle
      Integer.valueOf(fd)
    )
  )
  methods.put("close", function("close") { args => files.remove(args.head.asInt()).foreach(_.close()); null })
  methods.put("size", function("size")(args => java.lang.Long.valueOf(files(args.head.asInt()).length())))
  methods.put("pathSize", function("pathSize")(args => java.lang.Long.valueOf(Files.size(path(args.head.asString())))))
  methods.put(
    "read",
    function("read")(args =>
      val count = args(3).asInt()
      require(count >= 0 && count <= 1048576, "compiler read bound")
      val handle = files(args.head.asInt())
      if !args(4).isNull then handle.seek(args(4).asLong())
      val bytes = new Array[Byte](count)
      val read  = math.max(0, handle.read(bytes))
      (0 until read).foreach(index =>
        args(1).setArrayElement(args(2).asInt().toLong + index, Integer.valueOf(bytes(index) & 255))
      )
      Integer.valueOf(read)
    )
  )
  methods.put(
    "write",
    function("write")(args =>
      val count = args(3).asInt()
      require(count >= 0 && count <= 1048576, "compiler write bound")
      val bytes =
        Array.tabulate[Byte](count)(index => args(1).getArrayElement(args(2).asInt().toLong + index).asInt().toByte)
      args.head.asInt() match
        case 1  => stdout.write(bytes)
        case 2  => stderr.write(bytes)
        case fd =>
          val handle = files(fd)
          if !args(4).isNull then handle.seek(args(4).asLong())
          require(handle.getFilePointer + count <= 64L * 1024 * 1024, "compiler output file exceeds 64 MiB")
          handle.write(bytes)
      Integer.valueOf(count)
    )
  )
  methods.put("mkdir", function("mkdir") { args => Files.createDirectory(path(args.head.asString(), true)); null })
  methods.put("remove", function("remove") { args => Files.delete(path(args.head.asString(), true)); null })
  methods.put(
    "rename",
    function("rename") { args => Files.move(path(args.head.asString(), true), path(args(1).asString(), true)); null }
  )
  methods.put(
    "list",
    function("list")(args =>
      val stream = Files.list(path(args.head.asString()))
      try ProxyArray.fromArray(stream.toArray.map(_.asInstanceOf[Path].getFileName.toString.asInstanceOf[AnyRef])*)
      finally stream.close()
    )
  )
  val proxy: ProxyObject = ProxyObject.fromMap(methods)
  def close(): Unit      =
    files.values.foreach(_.close())
    files.clear()
