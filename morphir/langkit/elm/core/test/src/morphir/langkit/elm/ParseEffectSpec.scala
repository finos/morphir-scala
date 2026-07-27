package morphir.langkit.elm

import kyo.*
import kyo.kernel.ArrowEffect
import kyo.test.*

import morphir.langkit.elm.compiler.{DiagnosticCode, ParseDiagnostic}
import morphir.langkit.elm.cst.{CstModule, CstValueDeclaration}

/**
 * The parse pipeline as an effect: what a stage asks for, and what an interpreter may do about it.
 *
 * The point of suspending rather than returning is that these decisions belong to the interpreter — collecting every
 * report instead of stopping at the first, or handing back a tree the default interpreter would withhold.
 */
class ParseEffectSpec extends Test[Any]:

  private def source(declarations: String): String =
    s"module M exposing (..)\n\n$declarations\n"

  private def codes(outcome: Parse.Outcome[?]): List[String] =
    outcome.diagnostics.toList.map(r => DiagnosticCode.unwrap(r.diagnostic.code))

  "reporting" - {
    "every unresolvable chain in a module is reported, not just the first" in {
      val outcome = Elm.diagnoseCst(
        source("first = a == b == c\n\nsecond = d |> f <| g\n\nthird = h < i == j")
      )
      assert(codes(outcome) == List("ELM-P004", "ELM-P004", "ELM-P004"))
      assert(outcome.value.isEmpty)
    }

    "reports from different stages of the same module accumulate" in {
      val outcome = Elm.diagnoseCst(source("first = a <%> b\n\nsecond = c == d == e"))
      assert(codes(outcome) == List("ELM-P005", "ELM-P004"))
    }

    "a clean module reports nothing" in {
      val outcome = Elm.diagnoseCst(source("main = 1 + 2 * 3"))
      assert(outcome.diagnostics.isEmpty)
      assert(outcome.isSuccess)
    }

    "lenient options report what they had to guess, and still hand back the tree" in {
      val outcome = Elm.diagnoseCst(source("main = a == b == c"), ElmParseOptions.lenient)
      assert(codes(outcome) == List("ELM-P004"))
      assert(outcome.errors.isEmpty)
      assert(outcome.isSuccess)
    }
  }

  "halting" - {
    "a syntax failure halts, so nothing downstream runs" in {
      val outcome = Elm.diagnoseCst("module M exposing (..)\n\nmain = = =\n")
      assert(outcome.value.isEmpty)
      assert(outcome.errors.size == 1)
    }
  }

  "stages" - {
    "read their options from the effect rather than from a parameter" in {
      val strict  = Parse.run(ElmParseOptions.elm)(Parse.options)
      val lenient = Parse.run(ElmParseOptions.lenient)(Parse.options)
      assert(strict.value.contains(ElmParseOptions.elm))
      assert(lenient.value.contains(ElmParseOptions.lenient))
    }

    "compose with each other in the effect" in {
      val pipeline: Int < Parse =
        for
          module <- ElmParse.cst(source("main = 1 + 2 * 3"))
          count = module.declarations.count {
            case _: CstValueDeclaration => true
            case _                      => false
          }
        yield count

      assert(Parse.run(ElmParseOptions.elm)(pipeline).value.contains(1))
    }
  }

  "a different interpreter" - {

    /**
     * A second interpretation of the very same stages: collect the reports, but hand back the tree regardless of how
     * severe they were. This is what a linter or an editor wants, and it is written here rather than in the pipeline
     * because the pipeline has no opinion about it.
     */
    def keepTheTree[A](options: ElmParseOptions)(pipeline: A < Parse)(using Frame): (Chunk[Reported], Option[A]) =
      ArrowEffect.handleLoop(Tag[Parse], Chunk.empty[Reported], pipeline)(
        handle = [C] =>
          (input, state, cont) =>
            input match
              case ParseOp.Options          => Loop.continue(state, cont(options.asInstanceOf[C]))
              case ParseOp.Report(reported) => Loop.continue(state.append(reported), cont(().asInstanceOf[C]))
              case ParseOp.Halt(diagnostic) =>
                Loop.done((Chunk(Reported(diagnostic, Severity.Error)), None: Option[A])),
        done = (state, value) => (state, Some(value))
      ).eval

    "keeps a tree the shipped interpreter withholds, from identical stages and options" in {
      val text = source("main = a == b == c")

      val shipped = Parse.run(ElmParseOptions.elm)(ElmParse.cst(text))
      assert(shipped.value.isEmpty)

      val (reported, tree) = keepTheTree(ElmParseOptions.elm)(ElmParse.cst(text))
      assert(reported.map(r => DiagnosticCode.unwrap(r.diagnostic.code)).toList == List("ELM-P004"))
      assert(reported.forall(_.isError))
      assert(tree.isDefined)
    }

    "still cannot invent a tree where a stage halted" in {
      val (reported, tree) = keepTheTree(ElmParseOptions.elm)(ElmParse.cst("module M exposing (..)\n\nmain = = =\n"))
      assert(tree.isEmpty)
      assert(reported.size == 1)
    }

    "sees the same reports whichever façade ran it" in {
      val viaEffect = Parse.run(ElmParseOptions.elm)(ElmParse.cst(source("main = a == b == c")))
      val viaFacade = Elm.diagnoseCst(source("main = a == b == c"))
      assert(codes(viaEffect) == codes(viaFacade))
    }
  }

  "the plain façade" - {
    "reports the first error of a pipeline that reported several" in {
      Elm.parseCst(source("first = a == b == c\n\nsecond = d == e == f")) match
        case parsley.Failure(diagnostic: ParseDiagnostic) =>
          assert(DiagnosticCode.unwrap(diagnostic.code) == "ELM-P004")
        case parsley.Success(m: CstModule) =>
          throw new AssertionError(s"expected a failure, parsed: ${m.declarations}")
    }
  }
