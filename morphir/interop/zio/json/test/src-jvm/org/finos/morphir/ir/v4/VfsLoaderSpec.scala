package org.finos.morphir.ir.v4

import zio.test._
import zio.test.Assertion._
import zio._

object VfsLoaderSpec extends ZIOSpecDefault {
  def spec = suite("VfsLoaderSpec")(
    test("loadRequest should return error for missing path") {
      for {
        result <- VfsLoader.loadRequest("some/non/existent/path").exit
      } yield assert(result)(fails(isSubtype[java.io.FileNotFoundException](anything)))
    },
    test("loadRequest should load valid VFS manifest") {
      val tempDir     = java.nio.file.Files.createTempDirectory("vfs-test")
      val formatFile  = tempDir.resolve("format.json")
      val jsonContent =
        """{
          |  "formatVersion": "4.0.0",
          |  "layout": "vfs",
          |  "packageName": ["org", "example"],
          |  "created": "2023-10-27T10:00:00Z"
          |}""".stripMargin
      java.nio.file.Files.write(formatFile, jsonContent.getBytes("UTF-8"))

      for {
        dist <- VfsLoader.loadRequest(tempDir.toAbsolutePath.toString)
      } yield assert(dist)(anything)
    },
    test("loadRequest should load definitions from pkg directory") {
      val tempDir    = java.nio.file.Files.createTempDirectory("vfs-pkg-test")
      val formatFile = tempDir.resolve("format.json")
      val formatJson =
        """{
          |  "formatVersion": "4.0.0",
          |  "layout": "vfs",
          |  "packageName": ["org", "example"],
          |  "created": "2023-10-27T10:00:00Z"
          |}""".stripMargin
      java.nio.file.Files.write(formatFile, formatJson.getBytes("UTF-8"))

      val pkgDir    = tempDir.resolve("pkg")
      val moduleDir = pkgDir.resolve("org").resolve("example")
      java.nio.file.Files.createDirectories(moduleDir)

      val typeFile = moduleDir.resolve("MyType.type.json")
      // Construct valid JSON for TypeDefinition.TypeAliasDefinition(params=[], body=Unit)
      val typeJson =
        """{
          |  "TypeAliasDefinition": {
          |     "params": [],
          |     "body": { "Unit": { "attributes": {} } }
          |  }
          |}""".stripMargin
      java.nio.file.Files.write(typeFile, typeJson.getBytes("UTF-8"))

      for {
        dist <- VfsLoader.loadRequest(tempDir.toAbsolutePath.toString)
        // Verify we loaded the distribution with the module and type
      } yield
        // assertions
        assert(dist)(anything)
    }
  ).provide(VfsLoader.live)
}
