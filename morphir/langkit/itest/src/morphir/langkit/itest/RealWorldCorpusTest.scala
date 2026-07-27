package morphir.langkit.itest

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.junit.jupiter.api.Assumptions.assumeTrue
import org.junit.jupiter.api.{DisplayName, Test}

import morphir.langkit.elm.Elm

/**
 * Every Elm module of several real packages, parsed.
 *
 * The conformance corpus under `resources/fixtures/conformance` is written to exercise the grammar, which means it
 * exercises the grammar its author thought of. This one exercises the grammar Elm's own authors wrote, in packages that
 * have never heard of this parser: `elm/core`, `elm/html`, `elm/browser`, and Morphir's own Elm.
 *
 * The sources are not in this repository. `MorphirLangkitItestElmPackages` clones each package at a pinned tag during
 * the build and passes the directory in as a system property, so third-party code stays out of git while still being
 * read on every run.
 *
 * A module that fails here is a conformance defect until proven otherwise: these packages compile with `elm make`. The
 * first run of this test failed 73 of 458 modules, on three causes: six words reserved that Elm treats as ordinary
 * identifiers (`String.left` was unparseable), the empty record `{}`, and G5 — the indentation context. The first two
 * are fixed; the third is what [[knownG5Failures]] lists.
 */
class RealWorldCorpusTest {

  /**
   * Modules that fail only on G5, the indentation-context gap in `morphir/langkit/elm/conformance.html`.
   *
   * Every one of them writes a continuation line indented level with the expression or type it continues, rather than
   * past it — `-> Program () model msg` under a `sandbox :` annotation, `|> Result.mapError …` under the expression it
   * pipes. Elm measures indentation against the enclosing declaration, this parser against the expression's own first
   * token, and that is the whole difference.
   *
   * The list is a ratchet, not an excuse: nothing outside it may fail, and nothing in it may start passing without
   * being removed. Closing G5 empties it.
   */
  private val knownG5Failures: Set[String] = Set(
    "elm-browser/src/Browser.elm",
    "elm-browser/src/Debugger/History.elm",
    "elm-browser/src/Debugger/Main.elm",
    "elm-core/src/Dict.elm",
    "elm-core/src/Platform.elm",
    "elm-core/src/Task.elm",
    "finos-morphir-elm/cli/src/Morphir/Web/DevelopApp.elm",
    "finos-morphir-elm/src/Morphir/IR/Documented/Codec.elm",
    "finos-morphir-elm/src/Morphir/IR/Documented/CodecV1.elm",
    "finos-morphir-elm/src/Morphir/IR/Module/Codec.elm",
    "finos-morphir-elm/src/Morphir/IR/SDK/UUID.elm",
    "finos-morphir-elm/src/Morphir/IR/Source.elm",
    "finos-morphir-elm/src/Morphir/SDK/Dict.elm",
    "finos-morphir-elm/src/Morphir/SDK/UUID.elm",
    "finos-morphir-elm/src/Morphir/Snowpark/Backend.elm",
    "finos-morphir-elm/src/Morphir/Snowpark/LetMapping.elm",
    "finos-morphir-elm/src/Morphir/Snowpark/PatternMatchMapping.elm",
    "finos-morphir-elm/src/Morphir/Snowpark/RecordWrapperGenerator.elm",
    "finos-morphir-elm/src/Morphir/Snowpark/UserDefinedFunctionMapping.elm",
    "finos-morphir-elm/src/Morphir/Visual/Components/ModalComponent.elm",
    "finos-morphir-elm/src/Morphir/Visual/ViewApply.elm",
    "finos-morphir-elm/tests-integration/snowpark/model/src/CompanyAssets/Rules/DepreciationRules.elm",
    "finos-morphir-elm/tests-integration/spark/elm-tests/src/GenerateAntiqueAgeData.elm",
    "finos-morphir-elm/tests-integration/spark/elm-tests/src/GenerateAntiqueNameData.elm",
    "finos-morphir-elm/tests-integration/spark/elm-tests/src/GenerateAntiqueProductData.elm",
    "finos-morphir-elm/tests-integration/spark/elm-tests/src/GenerateAntiqueSSData.elm",
    "finos-morphir-elm/tests-integration/spark/elm-tests/src/GenerateAntiquesData.elm"
  )

  private def packageRoot: Option[Path] =
    Option(System.getProperty("morphir.langkit.elm.corpus.packages"))
      .map(Paths.get(_))
      .filter(Files.isDirectory(_))

  private def modules(root: Path): Seq[Path] =
    Using.resource(Files.walk(root)) { paths =>
      paths.iterator.asScala
        .filter(path => Files.isRegularFile(path) && path.getFileName.toString.endsWith(".elm"))
        .toVector
        .sorted
    }

  @Test
  @DisplayName("every module of every fetched Elm package parses")
  def everyModuleParses(): Unit = {
    val root = packageRoot
    assumeTrue(
      root.isDefined,
      "no fetched Elm packages: run through Mill so morphir.langkit.elm.corpus.packages is set"
    )

    val found = modules(root.get)
    assert(found.nonEmpty, s"no .elm files under ${root.get} — the fetch task produced nothing")

    val outcomes = found.map { module =>
      val name = root.get.relativize(module).toString.replace('\\', '/')
      name -> Elm.parseCst(Files.readString(module)).fold(
        diagnostic => Some(diagnostic.message.linesIterator.take(3).mkString(" ")),
        _ => None
      )
    }

    val unexpectedFailures = outcomes.collect {
      case (name, Some(message)) if !knownG5Failures.contains(name) => s"  $name\n    $message"
    }
    val nowPassing = outcomes.collect { case (name, None) if knownG5Failures.contains(name) => name }

    assert(
      unexpectedFailures.isEmpty,
      s"""${unexpectedFailures.size} of ${found.size} real-world modules failed to parse for a new reason:
         |${unexpectedFailures.take(20).mkString("\n")}
         |${if (unexpectedFailures.size > 20) s"  … and ${unexpectedFailures.size - 20} more" else ""}
         |
         |These packages compile with `elm make`, so each is a conformance defect. Fix it, or record it in
         |morphir/langkit/elm/conformance.html and add the module to knownG5Failures with the gap it belongs to.
         |""".stripMargin
    )

    assert(
      nowPassing.isEmpty,
      s"""${nowPassing.size} module(s) listed as known G5 failures now parse:
         |${nowPassing.map("  " + _).mkString("\n")}
         |
         |Remove them from knownG5Failures. If the list is now empty, G5 is closed: delete it, and take the row out of
         |morphir/langkit/elm/conformance.html.
         |""".stripMargin
    )
  }
}
