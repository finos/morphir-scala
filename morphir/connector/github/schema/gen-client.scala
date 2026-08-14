//| scalaVersion: 3.8.4
//| mvnDeps:
//| - com.github.ghostdogpr::caliban-codegen:3.1.5

import caliban.codegen.{Codegen, Options}
import java.nio.file.{Files, Path}
import zio.{Runtime, Unsafe}

/** Regenerates the checked-in Caliban *client* from the GitHub schema subset.
  *
  * Caliban's codegen API is ZIO (`Codegen.generate` returns `zio.Task`). This file is a Mill script, not mill
  * source, and it is not on the connector classpath. The generated `Client.scala` and the published module stay on
  * Kyo: `caliban-client` (no ZIO compile dependency) plus `kyo-http`. `kyo-caliban` is a GraphQL server and is not
  * used.
  *
  * From the repository root:
  * {{{
  * ./mill morphir/connector/github/schema/gen-client.scala
  * }}}
  */
def main(): Unit =
  val root    = workspaceRoot
  val options = Options(
    schemaPath = root.resolve("morphir/connector/github/schema/github-subset.graphql").toString,
    toPath = root
      .resolve("morphir/connector/github/src/morphir/connector/github/internal/Client.scala")
      .toString,
    fmtPath = Some(root.resolve(".scalafmt.conf").toString),
    headers = None,
    packageName = Some("morphir.connector.github.internal"),
    clientName = Some("Client"),
    genView = Some(false),
    effect = None,
    scalarMappings = Some(Map("URI" -> "String", "ID" -> "String")),
    imports = None,
    abstractEffectType = None,
    splitFiles = Some(false),
    enableFmt = Some(true),
    extensibleEnums = None,
    preserveInputNames = None,
    supportIsRepeatable = None,
    addDerives = None,
    envForDerives = None,
    excludeDeprecated = None,
    supportDeprecatedArgs = None
  )
  Unsafe.unsafe { implicit u =>
    Runtime.default.unsafe.run(Codegen.generate(options, Codegen.GenType.Client)).getOrThrowFiberFailure()
    ()
  }

private def workspaceRoot: Path =
  val start = Path.of(".").toAbsolutePath.normalize
  Iterator
    .iterate(start)(_.getParent)
    .takeWhile(_ != null)
    .find(dir => Files.isRegularFile(dir.resolve("build.mill")))
    .getOrElse(sys.error(s"could not find build.mill above $start"))
