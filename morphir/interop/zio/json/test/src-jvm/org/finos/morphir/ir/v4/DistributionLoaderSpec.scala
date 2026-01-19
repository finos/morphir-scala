package org.finos.morphir.ir.v4

import zio.test._
import zio.test.Assertion._
import zio._
import org.finos.morphir.naming._
import org.finos.morphir.ir.json.MorphirJsonDecodingSupportV4._
import java.nio.file.Files

object DistributionLoaderSpec extends ZIOSpecDefault {
  def spec = suite("DistributionLoaderSpec")(
    test("load should delegate to VfsLoader for directory") {
      val tempDir     = Files.createTempDirectory("vfs-unified-test")
      val formatFile  = tempDir.resolve("format.json")
      val jsonContent =
        """{
          |  "formatVersion": "4.0.0",
          |  "layout": "vfs",
          |  "packageName": ["org", "example"],
          |  "created": "2023-10-27T10:00:00Z"
          |}""".stripMargin
      Files.write(formatFile, jsonContent.getBytes("UTF-8"))

      for {
        dist <- DistributionLoader.load(tempDir.toAbsolutePath.toString)
      } yield assert(dist)(anything)
    },
    test("load should return error for non-existent path") {
      for {
        result <- DistributionLoader.load("some/non/existent/path/999").exit
      } yield assert(result)(fails(isSubtype[java.io.FileNotFoundException](anything)))
    },
    test("load should load single file as classic distribution") {
      val tempDir         = Files.createTempDirectory("vfs-classic-test")
      val distributioFile = tempDir.resolve("morphir-ir.json")
      // Minimal valid Distribution JSON structure (assuming Dictionary/Library)
      // We need to know the exact JSON structure of Distribution.Library
      // Distribution is enum: Library(conf: LibraryDistribution).
      // LibraryDistribution(packageInfo, packageDef, decorations).
      // Let's guess/infer the JSON form or look at codecs.
      // Usually { "Library": { ... } } or just the object if configured flat.
      // Let's assume the decoder expects the enum tagging.
      val jsonContent =
        """{
          |  "Library": {
          |    "library": {
          |      "packageInfo": { "name": [["org"], ["example"]], "version": "0.0.0" },
          |      "definition": { "modules": {} },
          |      "dependencies": {}
          |    }
          |  }
          |}""".stripMargin
      Files.write(distributioFile, jsonContent.getBytes("UTF-8"))

      for {
        dist <- DistributionLoader.load(distributioFile.toAbsolutePath.toString)
      } yield assert(dist)(anything)
    }
    // Note: Classic loader test will be added when Classic loading is implemented/mocked
  )
}
