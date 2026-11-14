package morphir.spikes.wasmhost.tools

import java.nio.file.Path

object Checks:
  def main(args: Array[String]): Unit =
    val settings = ujson.read(os.read(os.Path(Path.of(args(0)).toAbsolutePath)))
    val output   = os.Path(Path.of(args(1)).toAbsolutePath)
    os.makeDir.all(output)
    val fixtures       = os.Path(settings("fixtures").str)
    val protocolInputs = Seq(
      "notification" -> "{\"jsonrpc\":\"2.0\",\"method\":\"morphir.initialized\"}\n",
      "exit"         -> "{\"jsonrpc\":\"2.0\",\"method\":\"morphir.exit\"}\n",
      "guest-error"  -> "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"morphir.extension.describe\"}\n"
    )
    protocolInputs.foreach((name, input) => os.write(output / s"$name.input.jsonl", input))
    val receipts = settings("configurations").arr.map { config =>
      val id     = config("id").str
      val binary = os.Path(config("command")(0).str)
      val before = Distribution.hash(binary)
      val clean  = output / s"$id-clean"
      os.makeDir.all(clean)
      os.proc("tar", "-xzf", config("package").str, "-C", clean).call()
      val cleanConfig = ujson.read(config.render())
      cleanConfig("command") = ujson.Arr((clean / "host").toString)
      if config("backend").str == "extism-ffm" then cleanConfig("nativeLibrary") = (clean / "libextism.dylib").toString
      val packaged = Profile.run(
        cleanConfig,
        settings,
        output,
        s"$id-clean-package",
        Seq("probe", "--guest", (fixtures / "echo.wasm").toString),
        false
      )
      packaged("expectedSuccess") = true
      packaged("matchedExpectation") = packaged("exit").num == 0
      // Extism's default fuel can finish the runaway before a two-second parent deadline.
      // Its in-downcall cancellation is tested separately by safety; these test parent teardown.
      val terminationCutoffMs = if config("backend").str == "extism-ffm" then "500" else "2000"
      def run(
          name: String,
          command: Seq[String],
          expectedSuccess: Boolean,
          env: Map[String, String] = Map.empty
      ): ujson.Obj =
        val receipt = Profile.run(config, settings, output, s"$id-$name", command, false, env)
        receipt("expectedSuccess") = expectedSuccess
        receipt("matchedExpectation") = ((receipt("exit").num == 0) == expectedSuccess)
        receipt
      val checks = Seq(
        run("help", Seq("--help"), true),
        run("probe-help", Seq("probe", "--help"), true),
        run("invalid-option", Seq("probe", "--unknown-option"), false),
        run("invalid-parent", Seq("supervise", "--guest", "/missing/guest.wasm"), false),
        run(
          "callbacks",
          Seq("probe", "--guest", (fixtures / "echo.wasm").toString, "--requests", "100", "--cycles", "2"),
          true
        ),
        run("callback-failure", Seq("probe", "--guest", (fixtures / "callback-failure.wasm").toString), false),
        run("excessive-memory", Seq("probe", "--guest", (fixtures / "excessive-memory.wasm").toString), false),
        run(
          "bounded-growth",
          Seq("probe", "--guest", (fixtures / "bounded-growth.wasm").toString),
          config("backend").str != "graalwasm"
        ),
        run("unknown-kernel", Seq("probe", "--guest", (fixtures / "unknown-kernel.wasm").toString), false),
        run(
          "denied-filesystem",
          Seq("probe", "--guest", (fixtures / "denied-filesystem.wasm").toString),
          config("backend").str == "extism-ffm"
        ),
        run("denied-http", Seq("probe", "--guest", (fixtures / "denied-http.wasm").toString), false),
        run("oversized-response", Seq("probe", "--guest", (fixtures / "oversized-response.wasm").toString), false),
        run("unknown-import", Seq("probe", "--guest", (fixtures / "unknown-import.wasm").toString), false),
        run("wrong-signature", Seq("probe", "--guest", (fixtures / "wrong-signature.wasm").toString), false),
        run(
          "private-state",
          Seq("probe", "--guest", (fixtures / "private-state.wasm").toString, "--requests", "100", "--cycles", "2"),
          true
        ),
        run(
          "concurrent-state",
          Seq(
            "supervise",
            "--guest",
            (fixtures / "private-state.wasm").toString,
            "--concurrency",
            "2",
            "--quiet-ms",
            "0"
          ),
          true
        ),
        run("second", Seq("probe", "--guest", (fixtures / "second.wasm").toString, "--expected-answer", "43"), true),
        run(
          "safety",
          Seq(
            "safety",
            "--guest",
            (fixtures / "echo.wasm").toString,
            "--runaway",
            (fixtures / "runaway.wasm").toString
          ),
          true
        ),
        run(
          "deadline",
          Seq(
            "supervise",
            "--guest",
            (fixtures / "runaway.wasm").toString,
            "--timeout-ms",
            terminationCutoffMs,
            "--idle-ms",
            "0",
            "--quiet-ms",
            "0"
          ),
          false
        ),
        run(
          "start-deadline",
          Seq(
            "supervise",
            "--guest",
            (fixtures / "runaway-start.wasm").toString,
            "--timeout-ms",
            terminationCutoffMs,
            "--idle-ms",
            "0",
            "--quiet-ms",
            "0"
          ),
          false
        ),
        run(
          "cancel",
          Seq(
            "supervise",
            "--guest",
            (fixtures / "runaway.wasm").toString,
            "--cancel-after-ms",
            terminationCutoffMs,
            "--idle-ms",
            "0",
            "--quiet-ms",
            "0"
          ),
          false
        ),
        run("healthy-after-termination", Seq("probe", "--guest", (fixtures / "echo.wasm").toString), true)
      ) ++ protocolInputs.map { (name, _) =>
        run(
          name,
          Seq(
            "mep",
            "--guest",
            (fixtures / s"$name.wasm").toString,
            "--input",
            (output / s"$name.input.jsonl").toString
          ),
          name != "guest-error"
        )
      }
      Seq("deadline" -> "deadline", "start-deadline" -> "deadline", "cancel" -> "cancelled").foreach { (name, status) =>
        val result  = ujson.read(os.read(output / s"$id-$name.stdout.json"))
        val receipt = checks.find(_("name").str == s"$id-$name").get
        receipt("terminationCutoffMs") = terminationCutoffMs.toInt
        receipt("allReaped") = result("allReaped")
        receipt("observedStatuses") = ujson.Arr.from(result("workers").arr.map(_("status")))
        receipt("matchedExpectation") = result("allReaped").bool && result("workers").arr.nonEmpty &&
          result("workers").arr.forall(_("status").str == status)
      }
      require(
        !os.read(output / s"$id-invalid-parent.stderr.jsonl").contains("workers-started"),
        "invalid arguments started a worker"
      )
      val after = Distribution.hash(binary)
      require(before == after, "host executable changed between guest probes")
      val loaderChecks = if config("backend").str != "extism-ffm" then Seq.empty
      else
        val guest = Seq("probe", "--guest", (fixtures / "echo.wasm").toString)
        val wrong = output / s"$id-wrong-architecture.dylib"
        os.copy.over(os.Path(config("nativeLibrary").str), wrong)
        val file = new java.io.RandomAccessFile(wrong.toIO, "rw")
        try file.write(Array[Byte](0, 0, 0, 0, 0, 0, 0, 0))
        finally file.close()
        Seq(
          run("large-fuel", guest ++ Seq("--fuel", "5000000001"), true),
          run("fuel-reset", guest ++ Seq("--fuel", "10000", "--requests", "100"), true),
          run("fuel-exhaustion", Seq("probe", "--guest", (fixtures / "runaway.wasm").toString, "--fuel", "100"), false),
          run("missing-library", guest, false, Map("MORPHIR_EXTISM_LIBRARY" -> "/missing/libextism.dylib")),
          run("wrong-digest", guest, false, Map("MORPHIR_EXTISM_SHA256" -> "00")),
          run(
            "wrong-architecture",
            guest,
            false,
            Map("MORPHIR_EXTISM_LIBRARY" -> wrong.toString, "MORPHIR_EXTISM_SHA256" -> Distribution.hash(wrong))
          )
        )
      ujson.Obj(
        "id"     -> id,
        "scope"  -> "synthetic-early-gate",
        "status" ->
          (if (Seq(packaged) ++ checks ++ loaderChecks).forall(_("matchedExpectation").bool) then "pass" else "fail"),
        "executableSha256Before" -> before,
        "executableSha256After"  -> after,
        "guestSha256"            -> Distribution.hash(fixtures / "echo.wasm"),
        "secondGuestSha256"      -> Distribution.hash(fixtures / "second.wasm"),
        "checks"                 -> ujson.Arr.from(Seq(packaged) ++ checks ++ loaderChecks)
      )
    }
    os.write(output / "checks.json", ujson.Arr.from(receipts).render(indent = 2))
