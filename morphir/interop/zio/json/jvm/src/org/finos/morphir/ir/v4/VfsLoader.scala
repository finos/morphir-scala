package org.finos.morphir.ir.v4

import zio._
import zio.json._
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._
import org.finos.morphir.naming._
import org.finos.morphir.ir.json.MorphirJsonDecodingSupportV4._

/**
 * Loader for Morphir VFS distributions.
 *
 * This object provides methods to load a Morphir distribution from a file system path that adheres to the Morphir VFS
 * structure (V4).
 *
 * Example Usage:
 * {{{
 *   import org.finos.morphir.ir.v4.VfsLoader
 *   import zio._
 *
 *   val PROGRAM: Task[Unit] = for {
 *     dist <- VfsLoader.loadRequest("/path/to/my-morphir-project")
 *     _    <- Console.printLine(s"Loaded distribution: $dist")
 *   } yield ()
 * }}}
 */
object VfsLoader {

  // -- VfsManifest Codecs --
  implicit val distributionModeDecoder: JsonDecoder[DistributionMode] = JsonDecoder.string.map {
    case "classic" => DistributionMode.ClassicMode
    case "vfs"     => DistributionMode.VfsMode
    case _         => DistributionMode.VfsMode // Default or fail? Spec says defaults.
  }

  implicit val packageNameDecoder: JsonDecoder[PackageName] =
    JsonDecoder.list[String].map(strs => PackageName(Path.fromList(strs.map(Name.fromString))))
  implicit val vfsManifestDecoder: JsonDecoder[VfsManifest] = DeriveJsonDecoder.gen[VfsManifest]

  def loadRequest(path: String): Task[Distribution] =
    for {
      p <- ZIO.attempt(Paths.get(path))
      _ <- ZIO.fail(new java.io.FileNotFoundException(s"Path not found: $path")).unless(Files.exists(p))
      _ <- ZIO.fail(new IllegalArgumentException(s"Path is not a directory: $path")).unless(Files.isDirectory(p))

      formatFile = p.resolve("format.json")
      _ <-
        ZIO.fail(new java.io.FileNotFoundException(s"format.json not found in $path")).unless(Files.exists(formatFile))

      formatJson <- ZIO.attempt(Files.readString(formatFile))
      manifest   <- ZIO.fromEither(formatJson.fromJson[VfsManifest]).mapError(msg =>
        new RuntimeException(s"Failed to parse format.json: $msg")
      )

      dist <- manifest.layout match {
        case DistributionMode.ClassicMode =>
          // Classic mode loading (single file) - Not fully implemented yet
          ZIO.fail(new NotImplementedError("Classic mode VFS loading not implemented"))
        case DistributionMode.VfsMode =>
          loadVfsDistribution(p, manifest)
      }
    } yield dist

  private def loadVfsDistribution(rootPath: java.nio.file.Path, manifest: VfsManifest): Task[Distribution] = {
    val pkgPath = rootPath.resolve("pkg")
    for {
      pkgDef <- if (Files.exists(pkgPath)) loadPackage(pkgPath) else ZIO.succeed(PackageDefinition(Map.empty))
      pkgInfo = PackageInfo(manifest.packageName, "0.0.0")
    } yield Distribution.Library(LibraryDistribution(pkgInfo, pkgDef, Map.empty))
  }

  private def loadPackage(pkgRoot: java.nio.file.Path): Task[PackageDefinition] = {
    // Traverse recursively to find modules.
    // We walk the tree safely ensuring the stream is closed.
    ZIO.scoped {
      ZIO.fromAutoCloseable(ZIO.attempt(Files.walk(pkgRoot))).map { stream =>
        stream.iterator().asScala.filter(Files.isRegularFile(_)).toList
      }
    }.flatMap { files =>
      // Group files by parent directory (Module)
      val filesByModule = files.groupBy(_.getParent)

      ZIO.foreach(filesByModule) { case (dir, moduleFiles) =>
        val relPath = pkgRoot.relativize(dir)
        // Handle empty path case correctly
        val modulePath = if (relPath.toString == "") Path.empty else Path.fromString(relPath.toString)
        val moduleName = ModuleName(modulePath)

        for {
          types <- ZIO.foreach(moduleFiles.filter(_.toString.endsWith(".type.json"))) { file =>
            val nameStr = file.getFileName.toString.stripSuffix(".type.json")
            val name    = Name.fromString(nameStr)
            for {
              content <- ZIO.attempt(Files.readString(file))
              // Try to decode as fully wrapped AccessControlled[Documented[TypeDefinition]]
              // If fails, fallback to simple TypeDefinition (legacy/simple mode)
              decoded <- ZIO.fromEither(
                content.fromJson[AccessControlled[Documented[TypeDefinition]]]
                  .orElse(
                    content.fromJson[TypeDefinition].map(d => AccessControlled(Access.Public, Documented(None, d)))
                  )
              ).mapError(e => new RuntimeException(s"Error decoding $file: $e"))
            } yield (name, decoded)
          }

          values <- ZIO.foreach(moduleFiles.filter(_.toString.endsWith(".value.json"))) { file =>
            val nameStr = file.getFileName.toString.stripSuffix(".value.json")
            val name    = Name.fromString(nameStr)
            for {
              content <- ZIO.attempt(Files.readString(file))
               // Try to decode as fully wrapped AccessControlled[Documented[ValueDefinition]]
               // If fails, fallback to simple ValueDefinition
              decoded <- ZIO.fromEither(
                content.fromJson[AccessControlled[Documented[ValueDefinition]]]
                  .orElse(
                    content.fromJson[ValueDefinition].map(d => AccessControlled(Access.Public, Documented(None, d)))
                  )
              ).mapError(e => new RuntimeException(s"Error decoding $file: $e"))
            } yield (name, decoded)
          }

        } yield (moduleName -> AccessControlled(Access.Public, ModuleDefinition(types.toMap, values.toMap)))
      }
    }.map(modList => PackageDefinition(modList.toMap))
  }
}
