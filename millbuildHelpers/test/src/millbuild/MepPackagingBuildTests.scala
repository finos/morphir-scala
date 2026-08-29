package millbuild

import utest.*

object MepPackagingBuildTests extends TestSuite:
  val tests = Tests:
    test("provider version defaults to 0.1.0 and accepts a build override"):
      assert(MepProviderVersion.fromEnvironment(Map.empty) == "0.1.0")
      assert(MepProviderVersion.fromEnvironment(Map("MORPHIR_ELM_MEP_VERSION" -> "9.8.7")) == "9.8.7")

    test("native smoke process reports a bounded timeout distinctly"):
      val javaExecutable = os.Path(System.getProperty("java.home")) / "bin" /
        (if scala.util.Properties.isWin then "java.exe" else "java")
      val result = MepNativeImageSmoke.runProcess(
        command = Seq(
          javaExecutable.toString,
          "-cp",
          System.getProperty("java.class.path"),
          "millbuild.MepNativeImageSmokeHang"
        ),
        input = Array.emptyByteArray,
        environment = Map.empty,
        timeoutMillis = 100L
      )

      assert(result == MepNativeImageSmoke.ProcessResult.TimedOut(100L))

    test("native smoke resolves executable paths before spawning"):
      val directory  = os.temp.dir(prefix = "mep-native-smoke-")
      val executable = directory / "provider"
      os.write(executable, Array.emptyByteArray)

      val presentedPath =
        if scala.util.Properties.isWin then executable
        else {
          val alias = os.temp.dir(prefix = "mep-native-smoke-alias-") / "mill-workspace"
          java.nio.file.Files.createSymbolicLink(alias.toNIO, directory.toNIO)
          alias / executable.last
        }

      assert(MepNativeImageSmoke.commandPath(presentedPath) == executable.toNIO.toFile.getCanonicalPath)

object MepNativeImageSmokeHang:
  def main(args: Array[String]): Unit = Thread.sleep(30000L)
