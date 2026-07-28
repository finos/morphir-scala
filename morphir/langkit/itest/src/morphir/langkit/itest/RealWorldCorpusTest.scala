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
 * are fixed; the third is what [[knownFailures]] lists.
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
  /**
   * Modules that do not parse yet, each with the reason.
   *
   * A ratchet rather than an excuse: nothing outside this set may fail, and nothing in it may start passing without
   * being removed. It began at 27 entries and is now empty — every module of every fetched package parses.
   */
  private val knownFailures: Set[String] = Set.empty

  private def packageRoot: Option[Path] =
    Option(System.getProperty("morphir.langkit.elm.corpus.packages"))
      .map(Paths.get(_))
      .filter(Files.isDirectory(_))

  /**
   * Every `.elm` file with something in it.
   *
   * `morphir-elm` carries a handful of zero-length `.elm` files under its integration tests. Elm rejects an empty
   * module too — a module needs a header — so refusing them is correct rather than a gap, and they are no evidence
   * either way.
   */
  private def modules(root: Path): Seq[Path] =
    Using.resource(Files.walk(root)) { paths =>
      paths.iterator.asScala
        .filter(path => Files.isRegularFile(path) && path.getFileName.toString.endsWith(".elm"))
        .filter(Files.size(_) > 0)
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
    assumeTrue(
      !Files.exists(root.get.resolve("OFFLINE-SKIPPED")),
      "Mill ran with --offline, so the real-world Elm packages were not fetched and this test has nothing to read"
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
      case (name, Some(message)) if !knownFailures.contains(name) => s"  $name\n    $message"
    }
    val nowPassing = outcomes.collect { case (name, None) if knownFailures.contains(name) => name }

    assert(
      unexpectedFailures.isEmpty,
      s"""${unexpectedFailures.size} of ${found.size} real-world modules failed to parse for a new reason:
         |${unexpectedFailures.take(20).mkString("\n")}
         |${if (unexpectedFailures.size > 20) s"  … and ${unexpectedFailures.size - 20} more" else ""}
         |
         |These packages compile with `elm make`, so each is a conformance defect. Fix it, or record it in
         |morphir/langkit/elm/conformance.html and add the module to knownFailures with the gap it belongs to.
         |""".stripMargin
    )

    assert(
      nowPassing.isEmpty,
      s"""${nowPassing.size} module(s) listed as known failures now parse:
         |${nowPassing.map("  " + _).mkString("\n")}
         |
         |Remove them from knownFailures, and take the matching row out of morphir/langkit/elm/conformance.html.
         |""".stripMargin
    )
  }
}
