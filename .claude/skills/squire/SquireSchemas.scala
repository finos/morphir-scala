//| scalaVersion: 3.8.4
//| moduleDeps: [SquireModel.scala, SquireProcess.scala]

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.math.{BigDecimal as JBigDecimal, MathContext, RoundingMode}
import kyo.*
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

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
    try
      val direct = Yaml.decode[Structure.Value](yaml)
      Yaml.parse(yaml) match
        case Result.Success(node) =>
          direct match
            case Result.Success(value) => renderConverted(reconcileYamlNode(value, node))
            case Result.Panic(_: NumberFormatException) =>
              recoverOverflowingIntegers(yaml, node).flatMap { recovered =>
                Yaml.decode[Structure.Value](recovered) match
                  case Result.Success(value) => renderConverted(reconcileYamlNode(value, node))
                  case Result.Failure(error) => conversionFailure(error)
                  case Result.Panic(error)   => conversionFailure(error)
              }
            case Result.Failure(error) => conversionFailure(error)
            case Result.Panic(error)   => conversionFailure(error)
        case Result.Failure(error) => conversionFailure(error)
        case Result.Panic(error)   => conversionFailure(error)
    catch
      case NonFatal(error) => conversionFailure(error)

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

  private def reconcileYamlNode(value: Structure.Value, node: Yaml.Node): Structure.Value =
    val numericAnchors = scala.collection.mutable.Map.empty[String, Structure.Value]

    def loop(value: Structure.Value, node: Yaml.Node): Structure.Value =
      (value, node) match
        case (Structure.Value.Record(fields), Yaml.Node.Mapping(entries, _)) if fields.size == entries.size =>
          Structure.Value.Record(fields.zip(entries).map {
            case ((name, child), (Yaml.Node.Scalar(key, _), childNode)) if name == key =>
              name -> loop(child, childNode)
            case ((name, child), _) => name -> child
          })
        case (Structure.Value.Sequence(elements), Yaml.Node.Sequence(nodes, _)) if elements.size == nodes.size =>
          Structure.Value.Sequence(elements.zip(nodes).map(loop))
        case (decoded, Yaml.Node.Scalar(raw, meta)) =>
          val number = yamlNumber(raw, meta)
          val reconciled = number
            .map(number => if number.isFinite then Structure.Value.Decimal(number) else Structure.Value.Null)
            .getOrElse {
              decoded match
                case Structure.Value.Str(value) =>
                  repairQuotedBoundaryScalar(raw, meta).getOrElse(Structure.Value.Str(value))
                case other => other
            }
          (number, meta.anchor) match
            case (Some(_), Present(anchor)) => numericAnchors.update(anchor.value, reconciled)
            case _                          => ()
          reconciled
        case (decoded, Yaml.Node.Alias(anchor, _)) => numericAnchors.getOrElse(anchor.value, decoded)
        case _                                     => value

    loop(value, node)

  private final case class NumericPatch(start: Int, length: Int, replacement: String)

  private def recoverOverflowingIntegers(yaml: String, node: Yaml.Node): Result[SquireError, String] =
    numericPatches(yaml, node).flatMap { patches =>
      if patches.isEmpty then
        Result.Failure(SquireError.Failure("schemas", "could not reconcile YAML numeric scalar"))
      else
        val recovered = patches.sortBy(_.start).reverse.foldLeft(yaml) { (text, patch) =>
          text.substring(0, patch.start) + patch.replacement + text.substring(patch.start + patch.length)
        }
        Result.Success(recovered)
    }

  private def numericPatches(yaml: String, node: Yaml.Node): Result[SquireError, List[NumericPatch]] =
    node match
      case Yaml.Node.Mapping(entries, _) =>
        entries.foldLeft(Result.Success(List.empty[NumericPatch]): Result[SquireError, List[NumericPatch]]) {
          case (result, (_, child)) =>
            result.flatMap(current => numericPatches(yaml, child).map(current ++ _))
        }
      case Yaml.Node.Sequence(elements, _) =>
        elements.foldLeft(Result.Success(List.empty[NumericPatch]): Result[SquireError, List[NumericPatch]]) {
          case (result, child) => result.flatMap(current => numericPatches(yaml, child).map(current ++ _))
        }
      case Yaml.Node.Scalar(raw, meta) =>
        val overflow =
          if numericTag(meta) then
            Yaml.decode[Structure.Value](raw) match
              case Result.Panic(_: NumberFormatException) => yamlNumber(raw, meta)
              case _                                      => None
          else None
        overflow match
          case None => Result.Success(Nil)
          case Some(number) =>
            scalarStart(yaml, raw, meta.mark) match
              case Some(start) =>
                val replacement =
                  if number == Double.PositiveInfinity then ".inf"
                  else if number == Double.NegativeInfinity then "-.inf"
                  else java.lang.Double.toString(number)
                Result.Success(List(NumericPatch(start, raw.length, replacement)))
              case None =>
                Result.Failure(
                  SquireError.Failure(
                    "schemas",
                    "could not reconcile YAML numeric scalar",
                    Present(s"line ${meta.mark.line + 1}, column ${meta.mark.column + 1}")
                  )
                )
      case Yaml.Node.Alias(_, _) => Result.Success(Nil)

  private def scalarStart(yaml: String, raw: String, mark: Yaml.Mark): Option[Int] =
    val start   = mark.index
    val lineEnd = yaml.indexOf('\n', start) match
      case -1    => yaml.length
      case index => index
    Option
      .when(start >= 0 && start <= yaml.length)(yaml.indexOf(raw, start))
      .filter(index => index >= start && index + raw.length <= lineEnd)

  private def yamlNumber(value: String, meta: Yaml.ScalarMeta): Option[Double] =
    Option.when(numericTag(meta)) {
      Yaml.decode[Double](value) match
        case Result.Success(number) => Some(number)
        case _                      => None
    }.flatten

  private def numericTag(meta: Yaml.ScalarMeta): Boolean =
    meta.style == Yaml.ScalarStyle.Plain &&
      (meta.tag match
        case Absent => true
        case Present(tag) =>
          val value = tag.value
          value == "!!int" || value == "!!float" || value.endsWith(":int") || value.endsWith(":float"))

  private def renderConverted(value: Structure.Value): Result[SquireError, String] =
    try Result.Success(renderJson(rewriteTopLevelId(value), 0) + "\n")
    catch
      case error: SquireError => Result.Failure(error)
      case NonFatal(error)    => conversionFailure(error)

  private def renderJson(value: Structure.Value, depth: Int): String =
    value match
      case Structure.Value.Record(fields) => renderObject(fields, depth)
      case Structure.Value.VariantCase(name, value) =>
        renderObject(Chunk("name" -> Structure.Value.Str(name), "value" -> value), depth)
      case Structure.Value.Sequence(elements) => renderArray(elements, depth)
      case Structure.Value.MapEntries(entries) =>
        renderArray(entries.map { case (key, value) => Structure.Value.Sequence(Chunk(key, value)) }, depth)
      case Structure.Value.Str(value)     => Json.encode(value)
      case Structure.Value.Bool(value)    => Json.encode(value)
      case Structure.Value.Integer(value) => renderNumber(value.toDouble)
      case Structure.Value.Decimal(value) => renderNumber(value)
      case Structure.Value.BigNum(value)  => renderNumber(value.toDouble)
      case Structure.Value.Null           => "null"
      case Structure.Value.Bytes(_) | Structure.Value.Instant(_) | Structure.Value.Duration(_) =>
        throw SquireError.Failure("json", "value cannot be represented as deterministic JSON")

  private def renderObject(fields: Chunk[(String, Structure.Value)], depth: Int): String =
    if fields.isEmpty then "{}"
    else
      fields
        .map { case (name, value) =>
          s"${indent(depth + 1)}${Json.encode(name)}: ${renderJson(value, depth + 1)}"
        }
        .mkString("{\n", ",\n", s"\n${indent(depth)}}")

  private def renderArray(elements: Chunk[Structure.Value], depth: Int): String =
    if elements.isEmpty then "[]"
    else
      elements
        .map(value => s"${indent(depth + 1)}${renderJson(value, depth + 1)}")
        .mkString("[\n", ",\n", s"\n${indent(depth)}]")

  private def renderNumber(value: Double): String =
    if !value.isFinite then "null"
    else if value == 0d then "0"
    else
      val negative = value < 0d
      val decimal  = shortestDecimal(math.abs(value)).stripTrailingZeros
      val digits   = decimal.unscaledValue.abs.toString
      val exponent = decimal.precision - decimal.scale - 1
      val unsigned =
        if exponent >= 21 || exponent <= -7 then
          val significand = if digits.length == 1 then digits else s"${digits.head}.${digits.tail}"
          val sign        = if exponent >= 0 then "+" else ""
          s"$significand" + "e" + s"$sign$exponent"
        else if exponent < 0 then "0." + ("0" * (-exponent - 1)) + digits
        else if exponent + 1 >= digits.length then digits + ("0" * (exponent + 1 - digits.length))
        else digits.take(exponent + 1) + "." + digits.drop(exponent + 1)
      if negative then "-" + unsigned else unsigned

  private def shortestDecimal(value: Double): JBigDecimal =
    val exact    = new JBigDecimal(value)
    val exponent = exact.precision - exact.scale - 1
    (1 to 17).iterator
      .flatMap { precision =>
        val context = new MathContext(precision, RoundingMode.HALF_EVEN)
        val rounded = exact.round(context)
        val quantum = JBigDecimal.ONE.scaleByPowerOfTen(exponent - precision + 1)
        List(rounded, rounded.subtract(quantum), rounded.add(quantum))
          .map(_.stripTrailingZeros)
          .filter(candidate => candidate.signum > 0 && candidate.precision <= precision)
          .filter(candidate => candidate.doubleValue == value)
          .sortWith((left, right) => closerDecimal(left, right, exact))
          .headOption
      }
      .next()

  private def closerDecimal(left: JBigDecimal, right: JBigDecimal, exact: JBigDecimal): Boolean =
    val comparison = left.subtract(exact).abs.compareTo(right.subtract(exact).abs)
    if comparison != 0 then comparison < 0
    else
      val leftEven  = !left.unscaledValue.abs.testBit(0)
      val rightEven = !right.unscaledValue.abs.testBit(0)
      if leftEven != rightEven then leftEven else left.compareTo(right) < 0

  private def indent(depth: Int): String = "  " * depth

  private def conversionFailure[A](error: Throwable): Result[SquireError, A] =
    Result.Failure(
      SquireError.Failure(
        "schemas",
        "could not decode YAML schema",
        Present(Option(error.getMessage).getOrElse(error.getClass.getName))
      )
    )

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
    if result.exitCode == 0 || result.exitCode == 1 || isCapturedValidationFailure(result) then ()
    else
      Abort.fail(
        SquireError.Failure(
          "schemas",
          s"jsonschema $operation could not run (exit ${result.exitCode})",
          Present(output.trim)
        )
      )

  private def isCapturedValidationFailure(result: ProcessResult): Boolean =
    if result.exitCode != 2 || result.stdout.nonEmpty then false
    else
      val lines = result.stderr.linesIterator.toList
      normalizedValidatorTarget(result.request).exists { target =>
        lines match
          case header :: "error: Schema validation failure" :: details =>
            header == s"fail: $target" && (details.isEmpty || capturedValidationDetails(details))
          case _ => false
      }

  private def capturedValidationDetails(lines: List[String]): Boolean =
    lines.nonEmpty && lines.size % 3 == 0 && lines.grouped(3).forall {
      case List(message, instance, evaluation) =>
        message.length > 2 && message.startsWith("  ") && message.charAt(2) != ' ' &&
          instance.matches("""    at instance location ".*" \(line [0-9]+, column [0-9]+\)""") &&
          evaluation.startsWith("    at evaluate path \"") && evaluation.endsWith("\"")
      case _ => false
    }

  private def normalizedValidatorTarget(request: ProcessRequest): Option[String] =
    request.argv.lastOption.flatMap { target =>
      try
        val requested = java.nio.file.Paths.get(target)
        val base = request.cwd match
          case Present(path) => path.toJava
          case Absent        => java.nio.file.Paths.get(java.lang.System.getProperty("user.dir"))
        Some((if requested.isAbsolute then requested else base.resolve(requested)).toAbsolutePath.normalize.toString)
      catch
        case _: java.nio.file.InvalidPathException => None
    }

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
