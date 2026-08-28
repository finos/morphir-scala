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

object MepNativeImageSmokeHang:
  def main(args: Array[String]): Unit = Thread.sleep(30000L)
