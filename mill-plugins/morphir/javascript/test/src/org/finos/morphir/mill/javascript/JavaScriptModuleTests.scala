package org.finos.morphir.mill.javascript

import java.nio.file.Files

import mill.PathRef
import org.finos.morphir.mill.javascript.node.{NodeDistribution, NodeProcess}
import org.finos.morphir.mill.javascript.npm.NpmProcess
import org.finos.morphir.mill.javascript.node.NodeRuntimeModule
import org.finos.morphir.mill.toolchain.{AcquisitionSettings, ArchiveFormat}
import scala.compiletime.testing.typeCheckErrors
import utest.*

object JavaScriptModuleTests extends TestSuite {
  private def distribution(osName: String, osArch: String): NodeDistribution =
    NodeDistribution.resolve(osName, osArch).fold(message => throw new java.lang.AssertionError(message), identity)

  private def withTempDir[A](f: os.Path => A): A = {
    val directory = os.Path(Files.createTempDirectory("javascript-module-test"))
    try f(directory)
    finally os.remove.all(directory)
  }

  val tests = Tests {
    test("node distributions retain the exact supported archives and checksums") {
      assert(NodeDistribution.Version == "24.19.0")
      val expected = Seq(
        (
          "Mac OS X",
          "aarch64",
          "node-v24.19.0-darwin-arm64.tar.gz",
          "8294b7aa9b03997481c06babf1e8b270c859358f27da57a11509afe537ac381d"
        ),
        (
          "darwin",
          "arm64",
          "node-v24.19.0-darwin-arm64.tar.gz",
          "8294b7aa9b03997481c06babf1e8b270c859358f27da57a11509afe537ac381d"
        ),
        (
          "macos",
          "amd64",
          "node-v24.19.0-darwin-x64.tar.gz",
          "d1b5e999db158c62fe8f7267a4476b035d8bd93b1a605bac24a3f0dd166e3316"
        ),
        (
          "linux",
          "aarch64",
          "node-v24.19.0-linux-arm64.tar.gz",
          "d28c8a5bf0a808f0ed434a1dce8c54ae98f0371c0bd86ac58abc613f73e6643f"
        ),
        (
          "Linux",
          "x86_64",
          "node-v24.19.0-linux-x64.tar.gz",
          "f625d97cd707df4ff96254916fbc5ff014f09c09effe5a1e0ca8f6d41a8789d4"
        ),
        (
          "windows",
          "arm64",
          "node-v24.19.0-win-arm64.zip",
          "8502f4a50b458d4cc38ed8f2001556c2cd239d464920f74017926ccb1e1c157f"
        ),
        (
          "Windows 11",
          "amd64",
          "node-v24.19.0-win-x64.zip",
          "57f71ab3652e797d84acddc79c81cc9ff1c6ddb2a1974cdb83f00fee9bff4c73"
        )
      )

      expected.foreach { case (osName, osArch, archiveName, sha256) =>
        val resolved = distribution(osName, osArch)
        assert(resolved.version == NodeDistribution.Version)
        assert(resolved.archiveName == archiveName)
        assert(resolved.sha256 == sha256)
        assert(resolved.format == (if (archiveName.endsWith(".zip")) ArchiveFormat.Zip else ArchiveFormat.TarGz))
      }
      assert(NodeDistribution.resolve("freebsd", "x86_64").isLeft)
      assert(NodeDistribution.resolve("linux", "riscv64").isLeft)
      assert(NodeDistribution.resolve("24.19.1", "linux", "x86_64").isLeft)
    }

    test("node distribution exposes provisioned executable paths") {
      val unix = distribution("linux", "amd64")
      assert(unix.nodeRelativePath == os.rel / "bin" / "node")
      assert(unix.npmCliRelativePath == os.rel / "lib" / "node_modules" / "npm" / "bin" / "npm-cli.js")

      val windows = distribution("windows", "x86_64")
      assert(windows.nodeRelativePath == os.rel / "node.exe")
      assert(windows.npmCliRelativePath == os.rel / "node_modules" / "npm" / "bin" / "npm-cli.js")
    }

    test("Mill PathRef signatures do not traverse symbolic-link targets") {
      withTempDir { root =>
        val targetFile = root / "target.txt"
        val targetDir  = root / "target-directory"
        os.write(targetFile, "first")
        os.makeDir.all(targetDir)
        os.write(targetDir / "first.txt", "first")
        val tracked       = root / "tracked"
        val fileLink      = tracked / "file-link"
        val directoryLink = tracked / "directory-link"
        os.makeDir.all(tracked)
        val links = scala.util.Try {
          Files.createSymbolicLink(fileLink.toNIO, targetFile.toNIO)
          Files.createSymbolicLink(directoryLink.toNIO, targetDir.toNIO)
        }
        links.foreach { _ =>
          val signature = PathRef(tracked).sig
          os.write.over(targetFile, "second with different bytes")
          os.write(targetDir / "second.txt", "new target content")
          assert(PathRef(tracked).sig == signature)
        }
      }
    }

    test("npm tracked inputs reject root and nested directory symlinks before reading targets") {
      withTempDir { root =>
        val outside = root / "outside"
        os.makeDir.all(outside)
        os.write(outside / "oversized-secret", "target bytes must never be read")
        val rootLink = root / "root-link"
        val tracked  = root / "tracked"
        os.makeDir.all(tracked)
        Files.createSymbolicLink(rootLink.toNIO, outside.toNIO)
        Files.createSymbolicLink((tracked / "nested-link").toNIO, outside.toNIO)
        Seq(rootLink, tracked).foreach { input =>
          val error = scala.util.Try {
            NpmProcess.inputPathRefs(Seq(input), NpmProcess.InputLimits(maxFileBytes = 1))
          }.failed.get
          assert(error.isInstanceOf[IllegalArgumentException])
          assert(error.getMessage.contains("symbolic link"))
          assert(!error.getMessage.contains("exceeds 1 bytes"))
        }
      }
    }

    test("package binary runtime validation and round trip") {
      val location                 = PackageBinary.CallSite("build.mill", 17, "consumer.tools")
      given PackageBinary.CallSite = location
      val accepted                 = Seq("morphir-elm", "eslint.js", "tool_2", "A9")
      accepted.foreach { input =>
        val parsed = PackageBinary.parse(input)
        assert(parsed.map(_.value) == Right(input))
      }

      val rejected =
        Seq("", ".", "..", "...", "tool.", "a/b", "a\\b", "white space", "@scope/tool", "CON", "nul.txt", "Lpt9")
      rejected.foreach { input =>
        val error = PackageBinary.parse(input).swap.toOption.get
        assert(error.input == input)
        assert(error.location == location)
        assert(error.getMessage.contains(location.render))
      }
    }

    test("package binary literal validates at compile time and rejects interpolation") {
      val literal = packageBinary"morphir-elm"
      assert(literal.value == "morphir-elm")
      val invalid        = typeCheckErrors("""import org.finos.morphir.mill.javascript.*; packageBinary"../tool"""")
      val trailingPeriod =
        typeCheckErrors("""import org.finos.morphir.mill.javascript.*; packageBinary"tool."""")
      val allDots      = typeCheckErrors("""import org.finos.morphir.mill.javascript.*; packageBinary"..."""")
      val interpolated = typeCheckErrors(
        """import org.finos.morphir.mill.javascript.*; val name = "tool"; packageBinary"$name""""
      )
      assert(invalid.nonEmpty)
      assert(invalid.head.message.contains("package binary"))
      assert(invalid.head.column > 0)
      assert(trailingPeriod.nonEmpty)
      assert(trailingPeriod.head.message.contains("package binary"))
      assert(allDots.nonEmpty)
      assert(allDots.head.message.contains("package binary"))
      assert(interpolated.nonEmpty)
      assert(interpolated.head.message.contains("does not accept interpolation"))
    }

    test("runtime and npm commands never perform ambient executable lookup") {
      withTempDir { root =>
        val node    = PathRef(root / "provisioned" / "bin" / "node")
        val npm     = PathRef(root / "provisioned" / "lib" / "npm-cli.js")
        val runtime = NodeProcess.runtime(node, Seq("--version"))
        val manager = NodeProcess.npm(node, npm, Seq("--version"))
        assert(runtime == JavaScriptCommand(node, Seq("--version")))
        assert(manager == JavaScriptCommand(node, Seq(npm.path.toString, "--version")))
        assert(!runtime.arguments.exists(Set("node", "npm", "npx", "bun", "mise")))
        assert(!manager.arguments.exists(Set("node", "npm", "npx", "bun", "mise")))
      }
    }

    test("legacy Node aliases remain override-compatible") {
      val errors = typeCheckErrors(
        """
          import mill.*
          import org.finos.morphir.mill.javascript.node.NodeRuntimeModule
          trait CustomLegacyNode extends NodeRuntimeModule {
            override def nodeVersion: T[String] = runtimeVersion
            override def nodeHome: T[PathRef] = runtimeHome
            override def nodeExecutable: T[PathRef] = runtimeExecutable
          }
        """
      )
      assert(errors.isEmpty)
    }

    test("npm owns final safe tracked-input tasks") {
      val errors = typeCheckErrors(
        """
          import mill.*
          import org.finos.morphir.mill.javascript.npm.NpmPackageManagerModule
          trait UnsafeNpmInputs extends NpmPackageManagerModule {
            override def projectFiles: T[Seq[PathRef]] = ???
          }
        """
      )
      assert(errors.nonEmpty)
      assert(errors.exists(_.message.contains("final member")))
    }

    test("npm install requires a committed lock before process execution") {
      withTempDir { root =>
        val manifest = root / "package.json"
        os.write(manifest, "{}")
        val error = scala.util.Try {
          NpmProcess.prepareInstall(
            root / "install",
            NpmProcess.trackInputs(Seq(manifest), NpmProcess.InputKind.Project),
            Seq.empty
          )
        }.failed.get.asInstanceOf[IllegalArgumentException]
        assert(error.getMessage.contains("committed npm lock"))
        assert(!os.exists(root / "install"))
      }
    }

    test("npm install copies tracked project files and isolates state") {
      withTempDir { root =>
        val project = root / "project"
        os.makeDir.all(project)
        val manifest = project / "package.json"
        val lock     = project / "package-lock.json"
        os.write(manifest, "{}")
        os.write(lock, """{"lockfileVersion":3}""")
        val install = NpmProcess.prepareInstall(
          root / "task" / "install",
          NpmProcess.trackInputs(Seq(manifest), NpmProcess.InputKind.Project),
          NpmProcess.trackInputs(Seq(lock), NpmProcess.InputKind.Lock)
        )
        assert(install.projectFiles.map(_.path.last) == Seq("package.json"))
        assert(install.lockFiles.map(_.path.last) == Seq("package-lock.json"))
        assert(os.exists(install.root.path / "package.json"))
        assert(os.exists(install.root.path / "package-lock.json"))

        val environment = NpmProcess.environment(root / "task", Map("PATH" -> "/ambient", "HOME" -> "/ambient-home"))
        assert(!environment.contains("PATH"))
        assert(environment("HOME").startsWith((root / "task").toString))
        assert(environment("npm_config_cache").startsWith((root / "task").toString))
      }
    }

    test("npm install rejects source swaps before launch and cleans accepted state") {
      withTempDir { root =>
        def runCase(name: String)(mutate: (os.Path, os.Path) => Unit): Unit = {
          val caseRoot = root / name
          val project  = caseRoot / "project"
          val taskRoot = caseRoot / "task"
          val manifest = project / "package.json"
          val lock     = project / "package-lock.json"
          os.makeDir.all(project)
          os.write(manifest, "{}")
          os.write(lock, """{"lockfileVersion":3}""")
          val projectFiles = NpmProcess.trackInputs(Seq(manifest), NpmProcess.InputKind.Project)
          val lockFiles    = NpmProcess.trackInputs(Seq(lock), NpmProcess.InputKind.Lock)
          var launched     = false

          val error = scala.util.Try {
            NpmProcess.install(
              taskRoot,
              projectFiles,
              lockFiles,
              JavaScriptCommand(PathRef(caseRoot / "node"), Seq("npm-cli.js", "ci")),
              Map.empty,
              beforeVerify = () => mutate(manifest, lock),
              launch = (_, _, _) => launched = true
            )
          }.failed.get
          assert(error.isInstanceOf[IllegalArgumentException])
          assert(error.getMessage.contains("changed after its verified snapshot"))
          assert(!launched)
          assert(!os.exists(taskRoot / "install"))
          assert(!os.exists(taskRoot / "input-snapshot"))
          assert(!os.exists(taskRoot / "process-state"))
          assert(!os.exists(taskRoot / "npm-cache"))
        }

        runCase("content-swap") { (_, lock) =>
          os.write.over(lock, """{"lockfileVersion":3,"changed":true}""")
        }
        runCase("symlink-swap") { (manifest, _) =>
          val replacement = manifest / os.up / "replacement.json"
          os.write(replacement, "{}")
          os.remove(manifest)
          Files.createSymbolicLink(manifest.toNIO, replacement.toNIO)
        }
      }
    }

    test("npm input snapshots reject nested symlinks and bounded-input excess before launch") {
      withTempDir { root =>
        def attempt(
            name: String,
            limits: NpmProcess.InputLimits = NpmProcess.InputLimits()
        )(prepareDependency: os.Path => Unit): IllegalArgumentException = {
          val caseRoot   = root / name
          val project    = caseRoot / "project"
          val dependency = project / "fixture-tool"
          val manifest   = project / "package.json"
          val lock       = project / "package-lock.json"
          os.makeDir.all(dependency)
          os.write(manifest, "{}")
          os.write(lock, """{"lockfileVersion":3}""")
          prepareDependency(dependency)
          var launched = false
          val error    = scala.util.Try {
            NpmProcess.install(
              caseRoot / "task",
              NpmProcess.trackInputs(Seq(manifest, dependency), NpmProcess.InputKind.Project, limits),
              NpmProcess.trackInputs(Seq(lock), NpmProcess.InputKind.Lock, limits),
              JavaScriptCommand(PathRef(caseRoot / "node"), Seq("npm-cli.js", "ci")),
              Map.empty,
              limits = limits,
              launch = (_, _, _) => launched = true
            )
          }.failed.get.asInstanceOf[IllegalArgumentException]
          assert(!launched)
          assert(!os.exists(caseRoot / "task" / "install"))
          error
        }

        val outside = root / "outside-secret"
        os.write(outside, "must never be copied")
        val symlink = attempt("symlink") { dependency =>
          Files.createSymbolicLink((dependency / "secret-link").toNIO, outside.toNIO)
        }
        assert(symlink.getMessage.contains("symbolic link"))
        assert(symlink.getMessage.contains("secret-link"))

        val count = attempt("entry-limit", NpmProcess.InputLimits(maxEntries = 3)) { dependency =>
          os.write(dependency / "one.js", "one")
          os.write(dependency / "two.js", "two")
        }
        assert(count.getMessage.contains("entry count limit 3"))

        val bytes = attempt("byte-limit", NpmProcess.InputLimits(maxFileBytes = 4)) { dependency =>
          os.write(dependency / "large.js", "more than four bytes")
        }
        assert(bytes.getMessage.contains("exceeds 4 bytes"))
      }
    }

    test("stable verified npm inputs launch only from the snapshot-backed install") {
      withTempDir { root =>
        val project  = root / "project"
        val taskRoot = root / "task"
        val manifest = project / "package.json"
        val lock     = project / "package-lock.json"
        os.makeDir.all(project)
        os.write(manifest, "{}")
        os.write(lock, """{"lockfileVersion":3}""")
        var launched = false
        val install  = NpmProcess.install(
          taskRoot,
          NpmProcess.trackInputs(Seq(manifest), NpmProcess.InputKind.Project),
          NpmProcess.trackInputs(Seq(lock), NpmProcess.InputKind.Lock),
          JavaScriptCommand(PathRef(root / "node"), Seq("npm-cli.js", "ci")),
          Map.empty,
          launch = (_, cwd, _) => {
            launched = true
            assert(cwd == taskRoot / "install")
            assert(os.read(cwd / "package.json") == "{}")
            assert(os.read(cwd / "package-lock.json").contains("lockfileVersion"))
          }
        )
        assert(launched)
        assert(install.root.path == taskRoot / "install")
        assert(!os.exists(taskRoot / "input-snapshot"))
      }
    }

    test("npm commands are locked, local-only, and invalidation inputs remain explicit") {
      withTempDir { root =>
        val node    = PathRef(root / "node")
        val npm     = PathRef(root / "npm-cli.js")
        val install = JavaScriptInstall(
          PathRef(root / "install"),
          Seq(PathRef(root / "package.json")),
          Seq(PathRef(root / "package-lock.json"))
        )
        val installedPackage = install.root.path / "node_modules" / "fixture-tool"
        os.makeDir.all(installedPackage)
        os.write(installedPackage / "package.json", """{"name":"fixture-tool","bin":{"fixture-tool":"cli.js"}}""")
        os.write(installedPackage / "cli.js", "console.log('fixture')")
        val ci = NpmProcess.ci(node, npm, root / "npm-cache")
        assert(
          ci.arguments == Seq(
            npm.path.toString,
            "ci",
            "--ignore-scripts",
            "--no-audit",
            "--no-fund",
            "--cache",
            (root / "npm-cache").toString
          )
        )
        val binary = NpmProcess.binary(node, install, packageBinary"fixture-tool", Seq("answer"))
        assert(binary.executable == node)
        assert(binary.arguments.head.endsWith("node_modules/fixture-tool/cli.js"))
        assert(binary.arguments.endsWith(Seq("answer")))
        assert(!binary.arguments.exists(Set("npm", "npx", "--package", "--yes")))
      }
    }

    test("provisioned node performs locked npm ci and executes only the installed local binary") {
      withTempDir { root =>
        val fixtureResource = Option(getClass.getClassLoader.getResource("locked-npm-project/package.json"))
          .getOrElse(throw new java.lang.AssertionError("locked npm fixture is not on the test classpath"))
        val fixture      = os.Path(java.nio.file.Paths.get(fixtureResource.toURI)) / os.up
        val distribution = NodeDistribution
          .resolve(System.getProperty("os.name"), System.getProperty("os.arch"))
          .fold(message => throw new java.lang.AssertionError(message), identity)
        val runtime  = NodeRuntimeModule.provision(distribution, AcquisitionSettings(), root / "runtime")
        val node     = PathRef(runtime.path / distribution.nodeRelativePath)
        val npm      = PathRef(runtime.path / distribution.npmCliRelativePath)
        val prepared = NpmProcess.prepareInstall(
          root / "task" / "install",
          NpmProcess.trackInputs(
            Seq(fixture / "package.json", fixture / "fixture-tool"),
            NpmProcess.InputKind.Project
          ),
          NpmProcess.trackInputs(Seq(fixture / "package-lock.json"), NpmProcess.InputKind.Lock)
        )
        val environment = NpmProcess.environment(
          root / "task" / "state",
          Map("PATH" -> "/definitely-not-usable", "HOME" -> "/ambient-home")
        )
        NpmProcess.initialize(environment)
        val ci = NpmProcess.ci(node, npm, root / "task" / "npm-cache")
        os.proc(ci.executable.path.toString +: ci.arguments)
          .call(cwd = prepared.root.path, env = environment, propagateEnv = false)
        val binary = NpmProcess.binary(node, prepared, packageBinary"fixture-tool", Seq("answer"))
        val result = os.proc(binary.executable.path.toString +: binary.arguments)
          .call(cwd = root, env = environment, propagateEnv = false)
        assert(result.out.text().trim == "fixture:answer")
        assert(!environment.contains("PATH"))
      }
    }

    test("local binary resolution supports npm bin shapes and rejects path escape") {
      withTempDir { root =>
        val install     = JavaScriptInstall(PathRef(root / "install"), Seq.empty, Seq.empty)
        val nodeModules = install.root.path / "node_modules"
        val objectBin   = nodeModules / "object-tool"
        val scopedBin   = nodeModules / "@scope" / "scoped-tool"
        os.makeDir.all(objectBin)
        os.makeDir.all(scopedBin)
        os.write(objectBin / "package.json", """{"name":"object-tool","bin":{"object-tool":"cli.js"}}""")
        os.write(objectBin / "cli.js", "console.log('object')")
        os.write(scopedBin / "package.json", """{"name":"@scope/scoped-tool","bin":"cli.js"}""")
        os.write(scopedBin / "cli.js", "console.log('scoped')")
        val node          = PathRef(root / "node.exe")
        val objectCommand = NpmProcess.binary(node, install, packageBinary"object-tool", Seq.empty)
        val scopedCommand = NpmProcess.binary(node, install, packageBinary"scoped-tool", Seq.empty)
        assert(objectCommand.executable == node)
        assert(objectCommand.arguments.head.endsWith("object-tool/cli.js"))
        assert(scopedCommand.arguments.head.endsWith("@scope/scoped-tool/cli.js"))
        assert(!objectCommand.arguments.head.endsWith(".cmd"))

        val escape = nodeModules / "escape-tool"
        os.makeDir.all(escape)
        os.write(escape / "package.json", """{"name":"escape-tool","bin":{"escape-tool":"../outside.js"}}""")
        os.write(nodeModules / "outside.js", "console.log('outside')")
        val error = scala.util.Try {
          NpmProcess.binary(node, install, packageBinary"escape-tool", Seq.empty)
        }.failed.get
        assert(error.getMessage.contains("unsafe binary path"))
      }
    }

    test("local binary resolution rejects installed package symlink escape") {
      withTempDir { root =>
        val install     = JavaScriptInstall(PathRef(root / "install"), Seq.empty, Seq.empty)
        val nodeModules = install.root.path / "node_modules"
        val outside     = root / "outside-package"
        os.makeDir.all(nodeModules)
        os.makeDir.all(outside)
        os.write(outside / "package.json", """{"name":"linked-tool","bin":{"linked-tool":"cli.js"}}""")
        os.write(outside / "cli.js", "console.log('outside')")
        val symlink = scala.util.Try(Files.createSymbolicLink((nodeModules / "linked-tool").toNIO, outside.toNIO))
        symlink.foreach { _ =>
          val error = scala.util.Try {
            NpmProcess.binary(PathRef(root / "node"), install, packageBinary"linked-tool", Seq.empty)
          }.failed.get
          assert(error.getMessage.contains("escapes its install root"))
        }
      }
    }

    test("local binary discovery rejects a symlinked scope before enumeration") {
      withTempDir { root =>
        val install      = JavaScriptInstall(PathRef(root / "install"), Seq.empty, Seq.empty)
        val nodeModules  = install.root.path / "node_modules"
        val outsideScope = root / "outside-scope"
        val outsideTool  = outsideScope / "scope-tool"
        os.makeDir.all(nodeModules)
        os.makeDir.all(outsideTool)
        os.write(outsideTool / "package.json", """{"name":"@scope/scope-tool","bin":"cli.js"}""")
        os.write(outsideTool / "cli.js", "console.log('outside')")
        val scope = nodeModules / "@scope"
        Files.createSymbolicLink(scope.toNIO, outsideScope.toNIO)

        val error = scala.util.Try {
          NpmProcess.binary(PathRef(root / "node"), install, packageBinary"scope-tool", Seq.empty)
        }.failed.get
        assert(error.isInstanceOf[IllegalArgumentException])
        assert(error.getMessage.contains("symbolic link"))
        assert(error.getMessage.contains(scope.toString))
      }
    }

    test("local binary discovery caps installed package count") {
      withTempDir { root =>
        val install     = JavaScriptInstall(PathRef(root / "install"), Seq.empty, Seq.empty)
        val nodeModules = install.root.path / "node_modules"
        os.makeDir.all(nodeModules / "first-tool")
        os.makeDir.all(nodeModules / "second-tool")

        val error = scala.util.Try {
          NpmProcess.binary(
            PathRef(root / "node"),
            install,
            packageBinary"missing-tool",
            Seq.empty,
            NpmProcess.DiscoveryLimits(maxPackages = 1, maxManifestBytes = 1024)
          )
        }.failed.get
        assert(error.isInstanceOf[IllegalArgumentException])
        assert(error.getMessage.contains("package count limit 1"))
        assert(error.getMessage.contains(nodeModules.toString))
      }
    }

    test("local binary discovery bounds every root and scoped directory entry") {
      withTempDir { root =>
        def assertBounded(name: String, populate: os.Path => Unit): Unit = {
          val install     = JavaScriptInstall(PathRef(root / name / "install"), Seq.empty, Seq.empty)
          val nodeModules = install.root.path / "node_modules"
          os.makeDir.all(nodeModules)
          populate(nodeModules)
          val error = scala.util.Try {
            NpmProcess.binary(
              PathRef(root / "node"),
              install,
              packageBinary"missing-tool",
              Seq.empty,
              NpmProcess.DiscoveryLimits(maxPackages = 10, maxDiscoveryEntries = 2, maxManifestBytes = 1024)
            )
          }.failed.get
          assert(error.isInstanceOf[IllegalArgumentException])
          assert(error.getMessage.contains("discovery entry count limit 2"))
          assert(!error.getMessage.contains("Invalid installed npm package manifest"))
        }

        assertBounded(
          "root-flood",
          nodeModules => {
            Seq("ordinary-1", "ordinary-2", "ordinary-3").foreach(name => os.write(nodeModules / name, "x"))
            os.write(nodeModules / "later-package" / "package.json", "[", createFolders = true)
          }
        )
        assertBounded(
          "scope-flood",
          nodeModules => {
            val scope = nodeModules / "@scope"
            os.makeDir.all(scope)
            Seq("ordinary-1", "ordinary-2", "ordinary-3").foreach(name => os.write(scope / name, "x"))
            os.write(scope / "later-package" / "package.json", "[", createFolders = true)
          }
        )
      }
    }

    test("installed package manifests are size bounded before reading") {
      withTempDir { root =>
        val install     = JavaScriptInstall(PathRef(root / "install"), Seq.empty, Seq.empty)
        val packageRoot = install.root.path / "node_modules" / "large-tool"
        val manifest    = packageRoot / "package.json"
        os.makeDir.all(packageRoot)
        os.write(manifest, """{"name":"large-tool","padding":"xxxxxxxxxxxxxxxx","bin":"cli.js"}""")

        val error = scala.util.Try {
          NpmProcess.binary(
            PathRef(root / "node"),
            install,
            packageBinary"large-tool",
            Seq.empty,
            NpmProcess.DiscoveryLimits(maxPackages = 10, maxManifestBytes = 32)
          )
        }.failed.get
        assert(error.isInstanceOf[IllegalArgumentException])
        assert(error.getMessage.contains(manifest.toString))
        assert(error.getMessage.contains("exceeds 32 bytes"))
      }
    }

    test("installed package manifest shape failures retain the manifest path") {
      withTempDir { root =>
        val install     = JavaScriptInstall(PathRef(root / "install"), Seq.empty, Seq.empty)
        val packageRoot = install.root.path / "node_modules" / "shape-tool"
        val manifest    = packageRoot / "package.json"
        os.makeDir.all(packageRoot)
        os.write(packageRoot / "cli.js", "console.log('shape')")
        val malformed = Seq(
          "[]"                                                -> "$",
          """{"bin":"cli.js"}"""                              -> "$.name",
          """{"name":17,"bin":"cli.js"}"""                    -> "$.name",
          """{"name":"@/shape-tool","bin":"cli.js"}"""        -> "$.name",
          """{"name":"shape-tool","bin":17}"""                -> "$.bin",
          """{"name":"shape-tool","bin":{"shape-tool":17}}""" -> "$.bin.shape-tool"
        )

        malformed.foreach { case (json, diagnostic) =>
          os.write.over(manifest, json)
          val error = scala.util.Try {
            NpmProcess.binary(PathRef(root / "node"), install, packageBinary"shape-tool", Seq.empty)
          }.failed.get
          assert(error.isInstanceOf[IllegalArgumentException])
          assert(error.getMessage.contains(manifest.toString))
          assert(error.getMessage.contains(diagnostic))
        }
      }
    }

    test("installed package binaries reject cross-platform absolute paths and contextualize missing targets") {
      withTempDir { root =>
        val install     = JavaScriptInstall(PathRef(root / "install"), Seq.empty, Seq.empty)
        val packageRoot = install.root.path / "node_modules" / "path-tool"
        val manifest    = packageRoot / "package.json"
        os.makeDir.all(packageRoot)
        val rejected = Seq(
          "/tmp/cli.js",
          "C:/tool/cli.js",
          "C:\\tool\\cli.js",
          "\\\\server\\share\\cli.js",
          "missing.js"
        )
        rejected.foreach { path =>
          os.write.over(manifest, ujson.Obj("name" -> "path-tool", "bin" -> path).render())
          val error = scala.util.Try {
            NpmProcess.binary(PathRef(root / "node"), install, packageBinary"path-tool", Seq.empty)
          }.failed.get
          assert(error.isInstanceOf[IllegalArgumentException])
          assert(error.getMessage.contains("Installed npm package binary"))
          assert(error.getMessage.contains(path))
        }
      }
    }

    test("neutral consumer needs only runtime and package-manager contracts") {
      assert(NeutralJavaScriptConsumer.usesOnlyContracts != null)
    }
  }
}
