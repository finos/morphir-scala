package morphir.spikes.wasmhost.tools

import java.nio.file.{Files, Path}
import kyo.ffi.codegen.FfiGenerator
import scala.jdk.CollectionConverters.*

object Generate:
  def main(args: Array[String]): Unit =
    require(args.length == 3, "generate requires compiled declarations, classpath and output directory")
    val walk  = Files.walk(Path.of(args(0)))
    val files = try walk.iterator().asScala.filter(_.toString.endsWith(".tasty")).map(_.toString).toSeq
    finally walk.close()
    val output = Path.of(args(2))
    try
      val result = FfiGenerator.generate(
        files,
        args(1).split(java.io.File.pathSeparator).toSeq,
        output,
        FfiGenerator.Platform.JVM
      )
      require(result.traits.nonEmpty, "kyo-ffi found no binding declarations")
      Files.writeString(output.resolve("reachability-metadata.json"), result.reachabilityMetadata)
      result.files.foreach(println)
      result.warnings.foreach(System.err.println)
      val _ = Files.writeString(
        output.resolve("result.json"),
        ujson.Obj(
          "status"         -> "pass",
          "kyoVersion"     -> "1.0.0-RC7",
          "generatedFiles" -> ujson.Arr.from(result.files.map(_.toString))
        ).render(indent = 2)
      )
    catch
      case error: IllegalStateException =>
        Files.createDirectories(output)
        val _ = Files.writeString(
          output.resolve("result.json"),
          ujson.Obj(
            "status"     -> "blocked",
            "kyoVersion" -> "1.0.0-RC7",
            "error"      -> error.getMessage,
            "decision" -> "Use direct JDK FFM for the Extism candidate; preserve this declaration/generation experiment"
          ).render(indent = 2)
        )
        System.err.println(error.getMessage)
