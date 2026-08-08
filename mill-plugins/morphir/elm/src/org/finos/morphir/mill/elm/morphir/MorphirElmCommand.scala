package org.finos.morphir.mill.elm.morphir

import mill.Task
import org.finos.morphir.mill.javascript.*
import scala.util.Try

object MorphirElmCommand {
  def apply(packageManager: JavaScriptPackageManagerModule, arguments: Seq[String]): Task[JavaScriptCommand] =
    packageManager.packageBinaryCommand(packageBinary"morphir-elm", arguments)
}

object MorphirElmLock {
  private val DependencyFields    = Seq("dependencies", "devDependencies", "optionalDependencies", "peerDependencies")
  private val PlainRegistrySpec   = "[A-Za-z0-9*^~<>=|+_. -]+".r
  private val RegistryPackageName =
    "(?:@[a-z0-9][a-z0-9._-]*/[a-z0-9][a-z0-9._-]*|[a-z0-9][a-z0-9._-]*)".r

  def validate(path: os.Path): Unit = {
    val document = try ujson.read(os.read(path))
    catch {
      case error: Exception =>
        throw new IllegalArgumentException(s"Invalid Morphir Elm npm lock $path: ${error.getMessage}", error)
    }
    val json = objectValue(path, "$", document)
    json.get("lockfileVersion") match {
      case Some(ujson.Num(value)) if value == 3.0 => ()
      case _                                      => throw invalid(path, "$.lockfileVersion must be lockfileVersion 3")
    }
    val packages = json.get("packages") match {
      case Some(value: ujson.Obj) => value.obj
      case _                      => throw invalid(path, "$.packages must be an object")
    }

    val root =
      packageMetadata(path, "", packages.getOrElse("", throw invalid(path, "$.packages is missing the root package")))
    val rootDependencies = dependencyMap(path, "", "dependencies", root, required = true)
    rootDependencies.get("morphir-elm") match {
      case Some(version) if version == MorphirElmTool.Version => ()
      case _ => throw invalid(path, s"root morphir-elm dependency must be exactly ${MorphirElmTool.Version}")
    }

    val morphirPath = "node_modules/morphir-elm"
    val morphir     = packageMetadata(
      path,
      morphirPath,
      packages.getOrElse(morphirPath, throw invalid(path, s"$$.packages is missing $morphirPath"))
    )
    exactString(path, morphirPath, "version", morphir, MorphirElmTool.Version, "pinned version")
    exactString(path, morphirPath, "resolved", morphir, MorphirElmTool.Resolved, "pinned resolved")
    exactString(path, morphirPath, "integrity", morphir, MorphirElmTool.Integrity, "pinned integrity")

    packages.foreach { case (packagePath, value) =>
      val metadata = packageMetadata(path, packagePath, value)
      if (packagePath.nonEmpty) {
        metadata.get("link") match {
          case Some(ujson.Bool(true))  => throw invalid(path, s"linked dependency at $packagePath")
          case Some(ujson.Bool(false)) => ()
          case Some(_)                 => throw invalid(path, s"$$.packages.$packagePath.link must be a boolean")
          case None                    => ()
        }
        stringField(path, packagePath, "version", metadata)
        metadata.get("resolved") match {
          case Some(ujson.Str(url)) if url.startsWith("https://registry.npmjs.org/") => ()
          case Some(ujson.Str(_))                                                    =>
            throw invalid(path, s"registry package $packagePath requires an exact resolved registry URL")
          case _ => throw invalid(path, s"$$.packages.$packagePath.resolved must be a string")
        }
        validateIntegrity(path, packagePath, stringField(path, packagePath, "integrity", metadata))
        metadata.get("hasInstallScript") match {
          case Some(ujson.Bool(true))  => throw invalid(path, s"install scripts are forbidden at $packagePath")
          case Some(ujson.Bool(false)) => ()
          case Some(_) => throw invalid(path, s"$$.packages.$packagePath.hasInstallScript must be a boolean")
          case None    => ()
        }
      }
      DependencyFields.foreach(field => dependencyMap(path, packagePath, field, metadata, required = false))
    }
  }

  private def dependencyMap(
      path: os.Path,
      packagePath: String,
      field: String,
      metadata: collection.Map[String, ujson.Value],
      required: Boolean
  ): Map[String, String] =
    metadata.get(field) match {
      case Some(value: ujson.Obj) =>
        value.obj.iterator.map { case (name, spec) =>
          val dependency = spec match {
            case ujson.Str(text) => text
            case _               => throw invalid(path, s"$$.packages.$packagePath.$field.$name must be a string")
          }
          if (!isRegistrySpec(dependency))
            throw invalid(path, s"non-registry dependency $name at $packagePath.$field is forbidden: $dependency")
          name -> dependency
        }.toMap
      case Some(_)          => throw invalid(path, s"$$.packages.$packagePath.$field must be an object")
      case None if required => throw invalid(path, s"$$.packages.$packagePath.$field must be an object")
      case None             => Map.empty
    }

  private def isRegistrySpec(value: String): Boolean =
    value match {
      case _ if isPlainRegistrySpec(value) => true
      case _ if value.startsWith("npm:")   =>
        val alias       = value.stripPrefix("npm:")
        val versionAt   = alias.lastIndexOf('@')
        val packageName = if (versionAt > 0) alias.take(versionAt) else alias
        val version     = if (versionAt > 0) alias.drop(versionAt + 1) else "latest"
        RegistryPackageName.matches(packageName) && isPlainRegistrySpec(version)
      case _ => false
    }

  private def isPlainRegistrySpec(value: String): Boolean = {
    val normalized = value.trim
    normalized.nonEmpty && !normalized.startsWith(".") && PlainRegistrySpec.matches(value)
  }

  private def validateIntegrity(path: os.Path, packagePath: String, integrity: String): Unit = {
    val values = integrity.split("\\s+").toSeq.filter(_.nonEmpty)
    if (values.isEmpty) throw invalid(path, s"$$.packages.$packagePath.integrity requires SHA-512 integrity")
    values.foreach { value =>
      if (!value.startsWith("sha512-"))
        throw invalid(path, s"$$.packages.$packagePath.integrity requires SHA-512 integrity")
      val encoded = value.stripPrefix("sha512-")
      val decoded = Try(java.util.Base64.getDecoder.decode(encoded)).getOrElse {
        throw invalid(path, s"$$.packages.$packagePath.integrity has invalid SHA-512 base64")
      }
      if (decoded.length != 64)
        throw invalid(path, s"$$.packages.$packagePath.integrity SHA-512 digest must decode to exactly 64 bytes")
    }
  }

  private def packageMetadata(
      path: os.Path,
      packagePath: String,
      value: ujson.Value
  ): collection.Map[String, ujson.Value] =
    value match {
      case metadata: ujson.Obj => metadata.obj
      case _                   => throw invalid(path, s"$$.packages.$packagePath must be an object")
    }

  private def objectValue(path: os.Path, jsonPath: String, value: ujson.Value): collection.Map[String, ujson.Value] =
    value match {
      case objectValue: ujson.Obj => objectValue.obj
      case _                      => throw invalid(path, s"$jsonPath must be an object")
    }

  private def stringField(
      path: os.Path,
      packagePath: String,
      field: String,
      metadata: collection.Map[String, ujson.Value]
  ): String =
    metadata.get(field) match {
      case Some(ujson.Str(value)) => value
      case _                      => throw invalid(path, s"$$.packages.$packagePath.$field must be a string")
    }

  private def exactString(
      path: os.Path,
      packagePath: String,
      field: String,
      metadata: collection.Map[String, ujson.Value],
      expected: String,
      description: String
  ): Unit = {
    val actual = stringField(path, packagePath, field, metadata)
    if (actual != expected)
      throw invalid(path, s"$packagePath $description must be exactly $expected")
  }

  private def invalid(path: os.Path, detail: String): IllegalArgumentException =
    new IllegalArgumentException(s"Invalid Morphir Elm npm lock $path: $detail")
}
