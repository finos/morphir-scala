//| scalaVersion: 3.8.4
//| moduleDeps: [SquireModel.scala, SquireProcess.scala]

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import kyo.*
import scala.jdk.CollectionConverters.*

final case class SchemaOutcome(
    file: String,
    status: String,
    detail: Maybe[String] = Absent
) derives Schema

final case class SchemaReport(
    command: String,
    from: String,
    to: String,
    check: Boolean,
    ok: Boolean,
    outcomes: List[SchemaOutcome]
) derives Schema

object SquireSchemas:
  val DefaultFrom: String      = "kb/bundles/morphir/morphir-upstream/sources/website/static/schemas"
  val DefaultOut: String       = ".dev/out/squire/schemas"
  val DefaultDocuments: String = "kb/bundles/morphir/morphir-upstream/sources"

  def convert(yaml: String): Result[SquireError, String] =
    Yaml.decode[Structure.Value](yaml) match
      case Result.Success(directValue) =>
        val value = Yaml.parse(yaml) match
          case Result.Success(node) => reconcileYamlNode(directValue, node)
          case Result.Failure(_)    => directValue
        try Result.Success(SquireJson.pretty(normalizeForJson(rewriteTopLevelId(value))))
        catch
          case error: SquireError => Result.Failure(error)
      case Result.Failure(error) =>
        Result.Failure(
          SquireError.Failure("schemas", "could not decode YAML schema", Present(error.getMessage))
        )

  def build(from: Path, to: Path, all: Boolean): SchemaReport < (Sync & Abort[SquireError]) =
    fromResultEffect(Sync.defer(buildResult(from, to, all)))

  def compare(from: Path, to: Path, all: Boolean): SchemaReport < (Sync & Abort[SquireError]) =
    fromResultEffect(Sync.defer(compareResult(from, to, all)))

  def validate(
      schemaSources: Path,
      generatedSchemas: Path,
      documents: Path,
      runner: ProcessRunner,
      jsonschema: String = "jsonschema"
  ): SchemaReport < (Async & Sync & Abort[SquireError]) =
    for
      available      <- runner.run(ProcessRequest(Chunk(jsonschema, "--version")))
      _              <- ensureJsonschema(available)
      yamlFiles      <- Sync.defer(discover(schemaSources, includeAll = true, recursive = false))
      schemaOutcomes <- Kyo.foreach(yamlFiles) { file =>
        for
          result <- runner.run(ProcessRequest(Chunk(jsonschema, "metaschema", file.toString)))
          _      <- ensureValidatorResult(result, "metaschema")
        yield
          val valid = result.exitCode == 0
          SchemaOutcome(
            basename(file),
            if valid then "metaschema-valid" else "metaschema-invalid",
            if valid then Absent else Present("does not satisfy its own metaschema")
          )
      }
      documentFiles    <- Sync.defer(discoverJson(documents))
      documentOutcomes <- Kyo.foreach(documentFiles) { file =>
        documentMajor(file).flatMap {
          case None =>
            SchemaOutcome(fileRelativeTo(file, documents), "skipped", Present("not a complete Morphir IR document"))
          case Some(major) =>
            val schema = generatedSchemas / s"morphir-ir-v$major.json"
            schema.exists.flatMap { exists =>
              if !exists then
                SchemaOutcome(
                  fileRelativeTo(file, documents),
                  "skipped",
                  Present(s"no generated schema for formatVersion major $major")
                )
              else
                for
                  result <- runner.run(ProcessRequest(Chunk(jsonschema, "validate", schema.toString, file.toString)))
                  _      <- ensureValidatorResult(result, "validate")
                yield
                  val valid = result.exitCode == 0
                  SchemaOutcome(
                    fileRelativeTo(file, documents),
                    if valid then "valid" else "invalid",
                    if valid then Absent else Present(s"failed validation against v$major")
                  )
            }
        }
      }
    yield SchemaReport(
      "schemas-validate",
      schemaSources.toString,
      generatedSchemas.toString,
      check = true,
      ok = true,
      schemaOutcomes.toList ++ documentOutcomes.toList
    )

  def exitCode(report: SchemaReport): Int = if report.ok then 0 else 1

  def renderText(report: SchemaReport): String =
    if report.command == "schemas-validate" then renderValidation(report)
    else
      val rows = report.outcomes.map { outcome =>
        val mark   = if outcome.status == "identical" || outcome.status == "written" then "  " else "❌"
        val detail = outcome.detail.fold("")(value => s" — $value")
        f"$mark ${outcome.file}%-42s ${outcome.status}$detail"
      }
      val summary =
        if report.check && report.outcomes.nonEmpty && report.outcomes.forall(_.status == "missing") then
          s"no generated JSON under ${report.to}\n" +
            "--check compares a directory that holds both, such as a reference checkout;\n" +
            "to generate from the mirror instead, drop --check or run `mise run schemas:build`."
        else if report.check then
          val bad = report.outcomes.count(outcome => outcome.status == "missing" || outcome.status == "drifted")
          if bad == 0 then s"${report.outcomes.size} schema(s) in step with their YAML"
          else s"$bad of ${report.outcomes.size} schema(s) out of step"
        else s"wrote ${report.outcomes.size} schema(s) to ${report.to}"
      (rows :+ "" :+ summary).mkString("\n") + "\n"

  private def buildResult(from: Path, to: Path, all: Boolean): Result[SquireError, SchemaReport] =
    try
      val selected = discover(from, all, recursive = false)
      if selected.isEmpty then noMatches(from, all)
      else
        Files.createDirectories(to.toJava)
        selected
          .foldLeft(Result.Success(List.empty[SchemaOutcome]): Result[SquireError, List[SchemaOutcome]]) {
            (result, source) =>
              result.flatMap { outcomes =>
                convert(Files.readString(source.toJava, StandardCharsets.UTF_8)).map { generated =>
                  val target = to / jsonName(basename(source))
                  Files.writeString(target.toJava, generated, StandardCharsets.UTF_8)
                  outcomes :+ SchemaOutcome(basename(target), "written")
                }
              }
          }
          .map { outcomes =>
            SchemaReport("schemas-to-json", from.toString, to.toString, check = false, ok = true, outcomes)
          }
    catch
      case error: java.io.IOException => ioFailure("build schemas", error)
      case error: SecurityException   => ioFailure("build schemas", error)

  private def compareResult(from: Path, to: Path, all: Boolean): Result[SquireError, SchemaReport] =
    try
      val selected = discover(from, all, recursive = false)
      if selected.isEmpty then noMatches(from, all)
      else
        selected
          .foldLeft(Result.Success(List.empty[SchemaOutcome]): Result[SquireError, List[SchemaOutcome]]) {
            (result, source) =>
              result.flatMap { outcomes =>
                convert(Files.readString(source.toJava, StandardCharsets.UTF_8)).map { generated =>
                  val target  = to / jsonName(basename(source))
                  val outcome =
                    if !Files.isRegularFile(target.toJava) then
                      SchemaOutcome(basename(target), "missing", Present("no generated JSON beside the YAML"))
                    else if Files.readString(target.toJava, StandardCharsets.UTF_8) == generated then
                      SchemaOutcome(basename(target), "identical")
                    else SchemaOutcome(basename(target), "drifted", Present("regenerate it — the YAML has moved on"))
                  outcomes :+ outcome
                }
              }
          }
          .map { outcomes =>
            val ok = outcomes.forall(outcome => outcome.status != "missing" && outcome.status != "drifted")
            SchemaReport("schemas-to-json", from.toString, to.toString, check = true, ok, outcomes)
          }
    catch
      case error: java.io.IOException => ioFailure("compare schemas", error)
      case error: SecurityException   => ioFailure("compare schemas", error)

  private def rewriteTopLevelId(value: Structure.Value): Structure.Value =
    value match
      case Structure.Value.Record(fields) =>
        Structure.Value.Record(fields.map {
          case ("$id", Structure.Value.Str(id)) if id.endsWith(".yaml") =>
            "$id" -> Structure.Value.Str(id.stripSuffix(".yaml") + ".json")
          case field => field
        })
      case other => other

  private def normalizeForJson(value: Structure.Value): Structure.Value =
    value match
      case Structure.Value.Record(fields) =>
        Structure.Value.Record(fields.map((name, value) => name -> normalizeForJson(value)))
      case Structure.Value.VariantCase(name, value) =>
        Structure.Value.VariantCase(name, normalizeForJson(value))
      case Structure.Value.Sequence(elements) =>
        Structure.Value.Sequence(elements.map(normalizeForJson))
      case Structure.Value.MapEntries(entries) =>
        Structure.Value.MapEntries(entries.map((key, value) => normalizeForJson(key) -> normalizeForJson(value)))
      case Structure.Value.Decimal(value)
          if value.isFinite && value >= Long.MinValue.toDouble && value <= Long.MaxValue.toDouble &&
            value == value.toLong.toDouble =>
        Structure.Value.Integer(value.toLong)
      case other => other

  private def reconcileYamlNode(value: Structure.Value, node: Yaml.Node): Structure.Value =
    (value, node) match
      case (Structure.Value.Record(fields), Yaml.Node.Mapping(entries, _)) if fields.size == entries.size =>
        Structure.Value.Record(fields.zip(entries).map {
          case ((name, child), (Yaml.Node.Scalar(key, _), childNode)) if name == key =>
            name -> reconcileYamlNode(child, childNode)
          case ((name, child), _) => name -> child
        })
      case (Structure.Value.Sequence(elements), Yaml.Node.Sequence(nodes, _)) if elements.size == nodes.size =>
        Structure.Value.Sequence(elements.zip(nodes).map(reconcileYamlNode))
      case (Structure.Value.Str(decoded), Yaml.Node.Scalar(raw, meta)) =>
        repairQuotedBoundaryScalar(raw, meta).getOrElse(Structure.Value.Str(decoded))
      case _ => value

  private def repairQuotedBoundaryScalar(value: String, meta: Yaml.ScalarMeta): Option[Structure.Value.Str] =
    val hasQuotedSurface =
      meta.style == Yaml.ScalarStyle.Plain && value.length >= 2 &&
        ((value.head == '"' && value.last == '"') || (value.head == '\'' && value.last == '\''))
    Option.when(hasQuotedSurface) {
      Yaml.decode[Structure.Value](s"value: $value\nsentinel: null\n") match
        case Result.Success(Structure.Value.Record(fields)) =>
          fields.collectFirst { case ("value", decoded: Structure.Value.Str) => decoded }
        case _ => None
    }.flatten

  private def discover(directory: Path, includeAll: Boolean, recursive: Boolean): List[Path] =
    if !Files.isDirectory(directory.toJava) then Nil
    else
      val stream = if recursive then Files.walk(directory.toJava) else Files.list(directory.toJava)
      try
        stream.iterator.asScala
          .filter(path => Files.isRegularFile(path))
          .filter { path =>
            val name   = path.getFileName.toString
            val prefix = if includeAll then "morphir-" else "morphir-ir-"
            name.startsWith(prefix) && name.endsWith(".yaml")
          }
          .map(path => Path(path.toString))
          .toList
          .sortBy(basename)
      finally stream.close()

  private def discoverJson(directory: Path): List[Path] =
    if !Files.isDirectory(directory.toJava) then Nil
    else
      val stream = Files.walk(directory.toJava)
      try
        stream.iterator.asScala
          .filter(path => Files.isRegularFile(path) && path.getFileName.toString.endsWith(".json"))
          .map(path => Path(path.toString))
          .toList
          .sortBy(path => fileRelativeTo(path, directory))
      finally stream.close()

  private def documentMajor(file: Path): Option[String] < Sync =
    Sync.defer {
      try
        Json.decode[Structure.Value](Files.readString(file.toJava, StandardCharsets.UTF_8)) match
          case Result.Success(Structure.Value.Record(fields)) =>
            fields.find(_._1 == "formatVersion").flatMap { case (_, value) => major(value) }
          case _ => None
      catch case _: java.io.IOException => None
    }

  private def major(value: Structure.Value): Option[String] =
    val text = value match
      case Structure.Value.Str(value)     => value
      case Structure.Value.Integer(value) => value.toString
      case Structure.Value.Decimal(value) => value.toString
      case Structure.Value.BigNum(value)  => value.toString
      case _                              => ""
    val digits = text.takeWhile(_.isDigit)
    Option.when(digits.nonEmpty)(digits)

  private def fileRelativeTo(file: Path, base: Path): String =
    base.toJava.toAbsolutePath.normalize.relativize(file.toJava.toAbsolutePath.normalize).toString.replace('\\', '/')

  private def jsonName(yamlName: String): String = yamlName.stripSuffix(".yaml") + ".json"

  private def basename(path: Path): String = path.toJava.getFileName.toString

  private def pattern(all: Boolean): String = if all then "morphir-*.yaml" else "morphir-ir-*.yaml"

  private def noMatches[A](from: Path, all: Boolean): Result[SquireError, A] =
    Result.Failure(SquireError.Failure("schemas", s"no files matching ${pattern(all)} under $from"))

  private def ioFailure[A](operation: String, error: Throwable): Result[SquireError, A] =
    Result.Failure(SquireError.Failure("schemas", s"could not $operation", Present(error.getMessage)))

  private def ensureJsonschema(result: ProcessResult): Unit < Abort[SquireError] =
    if result.exitCode == 0 then ()
    else
      Abort.fail(
        SquireError.Failure(
          "schemas",
          "jsonschema is not on PATH — run 'mise install' first",
          Present((result.stderr + result.stdout).trim)
        )
      )

  private def ensureValidatorResult(result: ProcessResult, operation: String): Unit < Abort[SquireError] =
    val output = result.stderr + result.stdout
    if result.exitCode <= 1 || output.contains("Schema validation failure") then ()
    else
      Abort.fail(
        SquireError.Failure(
          "schemas",
          s"jsonschema $operation could not run (exit ${result.exitCode})",
          Present(output.trim)
        )
      )

  private def fromResultEffect[A](effect: Result[SquireError, A] < Sync): A < (Sync & Abort[SquireError]) =
    effect.flatMap {
      case Result.Success(value) => value
      case Result.Failure(error) => Abort.fail(error)
    }

  private def renderValidation(report: SchemaReport): String =
    val rows = report.outcomes.map { outcome =>
      val label = outcome.status match
        case "metaschema-valid"   => "ok  "
        case "metaschema-invalid" => "FAIL"
        case "valid"              => "ok  "
        case "invalid"            => "FAIL"
        case _                    => "skip"
      val detail = outcome.detail.fold("")(value => s" — $value")
      s"  $label  ${outcome.file}$detail"
    }
    val documents = report.outcomes.filterNot(_.status.startsWith("metaschema-"))
    val checked   = documents.count(outcome => outcome.status == "valid" || outcome.status == "invalid")
    val failures  = documents.count(_.status == "invalid")
    (rows :+ "" :+ s"$checked document(s) checked, $failures failing" :+ "Schema check completed successfully")
      .mkString("\n") + "\n"
