package org.finos.morphir.mill.javascript

import java.nio.file.Files

import mill.*
import mill.api.ExecResult
import mill.testkit.{TestRootModule, UnitTester}
import org.finos.morphir.mill.javascript.node.{NodeDistribution, NodeRuntimeModule}
import org.finos.morphir.mill.javascript.npm.{NpmPackageManagerModule, NpmProcess}
import org.finos.morphir.mill.toolchain.AcquisitionSettings
import utest.*

object NpmInstallInvalidationTests extends TestSuite {
  private final class InvalidationBuild(
      workspace: os.Path,
      provisionedRuntime: PathRef,
      distribution: NodeDistribution,
      includeFixtureTool: Boolean = true
  ) extends TestRootModule(workspace) { outer =>
    lazy val millDiscover = mill.api.Discover[this.type]

    object runtime extends NodeRuntimeModule {
      def versionFile: T[PathRef] = Task.Source(outer.moduleDir / "runtime-version.txt")

      override def runtimeVersion: T[String] = Task {
        os.read(versionFile().path).trim
      }

      override def runtimeHome: T[PathRef] = Task {
        provisionedRuntime
      }

      override def runtimeExecutable: T[PathRef] = Task {
        PathRef(provisionedRuntime.path / distribution.nodeRelativePath)
      }

      override def npmCli: T[PathRef] = Task {
        PathRef(provisionedRuntime.path / distribution.npmCliRelativePath)
      }
    }

    object packages extends NpmPackageManagerModule {
      def runtime = outer.runtime

      override def npmProjectPaths: Seq[os.Path] =
        Seq(outer.moduleDir / "package.json") ++
          Option.when(includeFixtureTool)(outer.moduleDir / "fixture-tool")
      override def npmLockPaths: Seq[os.Path] = Seq(outer.moduleDir / "package-lock.json")
    }
  }

  private def withTempDir[A](f: os.Path => A): A = {
    val directory = os.Path(Files.createTempDirectory("npm-install-invalidation-test"))
    try f(directory)
    finally os.remove.all(directory)
  }

  private def success[A](result: Either[ExecResult.Failing[A], UnitTester.Result[A]]): UnitTester.Result[A] =
    result.fold(failure => throw new java.lang.AssertionError(failure.toString), identity)

  private def fixture: os.Path = {
    val resource = Option(getClass.getClassLoader.getResource("locked-npm-project/package.json"))
      .getOrElse(throw new java.lang.AssertionError("locked npm fixture is not on the test classpath"))
    os.Path(java.nio.file.Paths.get(resource.toURI)) / os.up
  }

  val tests = Tests {
    test("Mill invalidates npm install for manifest lock and runtime input mutations") {
      withTempDir { root =>
        val distribution = NodeDistribution
          .resolve(System.getProperty("os.name"), System.getProperty("os.arch"))
          .fold(message => throw new java.lang.AssertionError(message), identity)
        val provisioned = NodeRuntimeModule.provision(distribution, AcquisitionSettings(), root / "provisioned")
        val sources     = root / "sources"
        os.makeDir.all(sources)
        os.copy.over(fixture / "package.json", sources / "package.json")
        os.copy.over(fixture / "package-lock.json", sources / "package-lock.json")
        os.copy.over(fixture / "fixture-tool", sources / "fixture-tool", createFolders = true)
        os.write(sources / "runtime-version.txt", "runtime-one")

        val module = new InvalidationBuild(root / "workspace", provisioned, distribution)
        UnitTester(module, sources).scoped { evaluator =>
          val initial = success(evaluator(module.packages.install))
          assert(initial.evalCount > 0)
          val cached = success(evaluator(module.packages.install))
          assert(cached.evalCount == 0)
          val sentinel = cached.value.root.path / "stale-install-output"
          os.write(sentinel, "must be cleared when install re-evaluates")
          val outputMutationCached = success(evaluator(module.packages.install))
          assert(outputMutationCached.evalCount == 0)
          assert(os.isFile(sentinel))

          val manifestPath = module.moduleDir / "package.json"
          val manifest     = os.read(manifestPath).replace(
            "\"private\": true,",
            "\"private\": true,\n  \"reviewMarker\": \"manifest-two\","
          )
          os.write.over(manifestPath, manifest)
          val manifestMutation = success(evaluator(module.packages.install))
          assert(!os.exists(sentinel))
          assert(manifestMutation.evalCount >= 1)
          assert(os.read(manifestMutation.value.root.path / "package.json").contains("manifest-two"))
          os.write(sentinel, "must be cleared after lock mutation")

          val lockPath = module.moduleDir / "package-lock.json"
          val lock     = os.read(lockPath).replace(
            "\"requires\": true,",
            "\"requires\": true,\n  \"reviewMarker\": \"lock-two\","
          )
          os.write.over(lockPath, lock)
          val lockMutation = success(evaluator(module.packages.install))
          assert(!os.exists(sentinel))
          assert(lockMutation.evalCount >= 1)
          assert(os.read(lockMutation.value.root.path / "package-lock.json").contains("lock-two"))
          os.write(sentinel, "must be cleared after runtime mutation")

          os.write.over(module.moduleDir / "runtime-version.txt", "runtime-two")
          val runtimeMutation = success(evaluator(module.packages.install))
          assert(runtimeMutation.evalCount >= 2)
          assert(!os.exists(sentinel))
          val recached = success(evaluator(module.packages.install))
          assert(recached.evalCount == 0)
        }
      }
    }

    test("Mill invalidates npm install when full lock fingerprints differ despite a legacy PathRef collision") {
      withTempDir { root =>
        val firstLock  = """{"lockfileVersion":3,"x":"00041123"}"""
        val secondLock = """{"lockfileVersion":3,"x":"00165045"}"""
        val lockPath   = root / "collision-check" / "package-lock.json"
        os.write(lockPath, firstLock, createFolders = true)
        val firstTracked = NpmProcess.trackInputs(Seq(lockPath), NpmProcess.InputKind.Lock).head
        os.write.over(lockPath, secondLock)
        val secondTracked = NpmProcess.trackInputs(Seq(lockPath), NpmProcess.InputKind.Lock).head
        assert(firstTracked.pathRef.sig == secondTracked.pathRef.sig)
        assert(firstTracked.fingerprint != secondTracked.fingerprint)
        val collisionManifest = lockPath / os.up / "package.json"
        os.write(collisionManifest, "{}")
        val staleError = scala.util.Try {
          NpmProcess.prepareInstall(
            root / "collision-check" / "install",
            NpmProcess.trackInputs(Seq(collisionManifest), NpmProcess.InputKind.Project),
            Seq(firstTracked)
          )
        }.failed.get
        assert(staleError.isInstanceOf[IllegalArgumentException])
        assert(staleError.getMessage.contains("full fingerprint changed"))
        assert(!os.exists(root / "collision-check" / "install"))

        val distribution = NodeDistribution
          .resolve(System.getProperty("os.name"), System.getProperty("os.arch"))
          .fold(message => throw new java.lang.AssertionError(message), identity)
        val provisioned = NodeRuntimeModule.provision(distribution, AcquisitionSettings(), root / "provisioned")
        val sources     = root / "sources"
        os.makeDir.all(sources)
        os.write(sources / "package.json", """{"private":true}""")
        os.write(sources / "package-lock.json", firstLock)
        os.write(sources / "runtime-version.txt", "runtime-one")

        val module = new InvalidationBuild(root / "workspace", provisioned, distribution, includeFixtureTool = false)
        UnitTester(module, sources).scoped { evaluator =>
          val initial = success(evaluator(module.packages.install))
          assert(initial.evalCount > 0)
          val cached = success(evaluator(module.packages.install))
          assert(cached.evalCount == 0)
          val sentinel = cached.value.root.path / "stale-collision-output"
          os.write(sentinel, "must be cleared for a full-fingerprint mutation")

          os.write.over(module.moduleDir / "package-lock.json", secondLock)
          val mutation = success(evaluator(module.packages.install))
          assert(mutation.evalCount >= 1)
          assert(!os.exists(sentinel))
          assert(os.read(mutation.value.root.path / "package-lock.json") == secondLock)
        }
      }
    }
  }
}
