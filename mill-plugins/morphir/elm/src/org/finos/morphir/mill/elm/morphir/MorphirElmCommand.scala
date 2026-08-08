package org.finos.morphir.mill.elm.morphir

import mill.Task
import org.finos.morphir.mill.javascript.*

object MorphirElmCommand {
  def apply(packageManager: JavaScriptPackageManagerModule, arguments: Seq[String]): Task[JavaScriptCommand] =
    packageManager.packageBinaryCommand(packageBinary"morphir-elm", arguments)
}

object MorphirElmLock {
  def validate(path: os.Path): Unit = {
    val json = try ujson.read(os.read(path)).obj
    catch {
      case error: Exception =>
        throw new IllegalArgumentException(s"Invalid Morphir Elm npm lock $path: ${error.getMessage}", error)
    }
    val packages = json.get("packages") match {
      case Some(value: ujson.Obj) => value.obj
      case _                      => throw invalid(path, "missing packages object")
    }
    packages.foreach { case (packagePath, value) =>
      val metadata = value.obj
      if (packagePath.nonEmpty) {
        if (metadata.get("link").contains(ujson.Bool(true)))
          throw invalid(path, s"linked dependency at $packagePath")
        metadata.get("resolved") match {
          case Some(ujson.Str(url)) if url.startsWith("https://registry.npmjs.org/") => ()
          case _ => throw invalid(path, s"registry package $packagePath requires an exact resolved registry URL")
        }
        metadata.get("integrity") match {
          case Some(ujson.Str(value)) if value.startsWith("sha512-") && value.length > "sha512-".length => ()
          case _ => throw invalid(path, s"registry package $packagePath requires exact integrity")
        }
        if (metadata.get("hasInstallScript").contains(ujson.Bool(true)))
          throw invalid(path, s"install scripts are forbidden at $packagePath")
        rejectDependencySpecs(path, packagePath, metadata)
      } else rejectDependencySpecs(path, "root", metadata)
    }
  }

  private def rejectDependencySpecs(
      path: os.Path,
      packagePath: String,
      metadata: collection.Map[String, ujson.Value]
  ): Unit =
    Seq("dependencies", "devDependencies", "optionalDependencies").foreach { field =>
      metadata.get(field).foreach { value =>
        value.obj.foreach { case (name, spec) =>
          val dependency = spec.str
          if (
            dependency.startsWith("git") || dependency.startsWith("file:") || dependency.startsWith("link:") ||
            dependency.startsWith("http:") || dependency.startsWith("https:")
          ) throw invalid(path, s"non-registry dependency $name at $packagePath is forbidden: $dependency")
        }
      }
    }

  private def invalid(path: os.Path, detail: String): IllegalArgumentException =
    new IllegalArgumentException(s"Invalid Morphir Elm npm lock $path: $detail")
}
