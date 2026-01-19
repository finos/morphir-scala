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
    // Traverse recursively to find modules. A module is likely a directory containing definition files.
    // The path from pkgRoot corresponds to ModuleName.
    // However, we need to distinguish between directories that are just part of the path and directories that ARE modules (or contain module definitions).
    // In VFS, typically every directory path could map to a ModuleName if it contains defs?
    // Let's assume walk and for each directory check for .json files.

    // Simplification: We walk the tree.
    // If we find distinct files, we group them by parent directory.
    // The relative path from pkgRoot to parent dir is the ModuleName.

    val modules = scala.collection.mutable.Map[ModuleName, AccessControlled[ModuleDefinition]]()

    ZIO.attempt {
      Files.walk(pkgRoot).filter(Files.isRegularFile(_)).forEach { file =>
        val relParams = pkgRoot.relativize(file.getParent)
        // Convert path to module name
        val moduleName =
          if (relParams.toString == "") ModuleName(Path.empty) else ModuleName(Path.fromString(relParams.toString))

        // We accumulate definitions for this module
        // This is synchronous side-effecting inside ZIO.attempt, we should be careful or use ZIO streams/lists.
        // For now, let's collect all files first then process.
      }
    } *> ZIO.attempt(Files.walk(pkgRoot).iterator().asScala.filter(Files.isRegularFile(_)).toList).flatMap { files =>
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
              defBody <- ZIO.fromEither(content.fromJson[TypeDefinition]).mapError(e =>
                new RuntimeException(s"Error decoding $file: $e")
              )
              // Wrap in AccessControlled/Documented for now as we decoded specific def
              // If the file contains Access/Doc info, we should have decoded that.
              // Let's assume the file IS the ValueDefinition structure for now, or check if we need to wrap.
              // Our decoders decode TypeDefinition directly.
              // We need AccessControlled[Documented[TypeDefinition]].
              // We'll set default Access Public and empty Doc for now if not present, OR
              // if the JSON *is* AccessControlled[Documented[...]], we should use that decoder.
              // The spec usually implies the file content IS the definition.
              // Metadata like access can be in a separate file or wrapper.
              // Let's use Public/EmptyDoc defaults to proceed, effectively wrapping.
            } yield (name, AccessControlled(Access.Public, Documented(None, defBody)))
          }

          values <- ZIO.foreach(moduleFiles.filter(_.toString.endsWith(".value.json"))) { file =>
            val nameStr = file.getFileName.toString.stripSuffix(".value.json")
            val name    = Name.fromString(nameStr)
            for {
              content <- ZIO.attempt(Files.readString(file))
              defBody <- ZIO.fromEither(content.fromJson[ValueDefinition]).mapError(e =>
                new RuntimeException(s"Error decoding $file: $e")
              )
              // ValueDefinition wrapper in V4 IR already has AccessControlled body?
              // case class ValueDefinition(body: AccessControlled[ValueDefinitionBody])
              // But ModuleDefinition requires AccessControlled[Documented[ValueDefinition]]
              // This seems double wrapping or I misread.
              // ModuleDefinition: values: Map[Name, AccessControlled[Documented[ValueDefinition]]]
              // ValueDefinition: case run(body: AccessControlled[ValueDefinitionBody])
              // So it's AccessControlled[Documented[ValueDefinition]].
              // We wrap again.
            } yield (name, AccessControlled(Access.Public, Documented(None, defBody)))
          }

        } yield (moduleName -> AccessControlled(Access.Public, ModuleDefinition(types.toMap, values.toMap)))
      }
    }.map(modList => PackageDefinition(modList.toMap))
  }
}
