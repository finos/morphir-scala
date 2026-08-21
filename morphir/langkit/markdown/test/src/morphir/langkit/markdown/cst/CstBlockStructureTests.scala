package morphir.langkit.markdown.cst

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.HeadingLevel

/**
 * That graduated blocks come back typed, and ungraduated regions stay verbatim.
 *
 * The round-trip suite proves nothing is lost; this suite proves something is gained — a parse where every node were
 * verbatim would round-trip perfectly and mean nothing. One test per graduated form, plus the negative cases that pin
 * what lc8.22 and lc8.23 still own: containers and HTML stay verbatim, and a construct inside a container is not
 * recorded at top level.
 */
class CstBlockStructureTests extends Test[Any]:

  private def blocks(source: String): Chunk[CstNode] = CstParser.parse(source).children

  private def tokenTexts(node: CstNode): Chunk[String] =
    node.childNodes.collect { case CstNode.Token(text, _) => text }

  "graduated blocks" - {

    "a thematic break is a typed node holding one token" in {
      blocks("***\n") match
        case Chunk(CstNode.ThematicBreak(_, _), CstNode.Verbatim("\n", _)) => assert(true)
        case other => assert(false, s"expected ThematicBreak + newline gap, got $other")
    }

    "an ATX heading keeps its level and its marker token" in {
      blocks("## Hi\n").head match
        case CstNode.AtxHeading(level, _, _) =>
          assert(level == HeadingLevel.Two)
          assert(tokenTexts(blocks("## Hi\n").head).exists(_.startsWith("##")))
        case other => assert(false, s"expected AtxHeading, got $other")
    }

    "an ATX heading keeps its closing sequence as a token" in {
      blocks("# Hi #\n").head match
        case CstNode.AtxHeading(_, _, _) =>
          assert(tokenTexts(blocks("# Hi #\n").head).size == 2)
        case other => assert(false, s"expected AtxHeading, got $other")
    }

    "a setext heading keeps its underline as a token" in {
      blocks("Hi\n===\n").head match
        case CstNode.SetextHeading(level, _, _) =>
          assert(level == HeadingLevel.One)
          assert(tokenTexts(blocks("Hi\n===\n").head).exists(_.contains("===")))
        case other => assert(false, s"expected SetextHeading, got $other")
    }

    "a closed fence is token, text, token" in {
      blocks("```scala\ncode\n```\n").head match
        case CstNode.FencedCode(children, _) =>
          children.toSeq match
            case Seq(CstNode.Token(open, _), CstNode.Text(_, _), CstNode.Token(close, _)) =>
              assert(open.startsWith("```"))
              assert(close.startsWith("```"))
            case other => assert(false, s"expected token/text/token, got $other")
        case other => assert(false, s"expected FencedCode, got $other")
    }

    "an unterminated fence has no closing token" in {
      blocks("```\ncode\n").head match
        case CstNode.FencedCode(children, _) =>
          assert(children.collect { case t: CstNode.Token => t }.size == 1)
        case other => assert(false, s"expected FencedCode, got $other")
    }

    "indented code is typed, interior still verbatim" in {
      blocks("    code\n").head match
        case CstNode.IndentedCode(_, _) => assert(true)
        case other                      => assert(false, s"expected IndentedCode, got $other")
    }

    "a paragraph is typed, interior still verbatim until inlines graduate" in {
      blocks("plain prose\n").head match
        case CstNode.Paragraph(_, _) => assert(true)
        case other                   => assert(false, s"expected Paragraph, got $other")
    }
  }

  "not yet graduated" - {

    "a block quote stays verbatim" in
      assert(blocks("> quoted\n").forall {
        case _: CstNode.Verbatim => true
        case _                   => false
      })

    "a list stays verbatim" in
      assert(blocks("- item\n").forall {
        case _: CstNode.Verbatim => true
        case _                   => false
      })

    "a heading inside a quote is the quote's problem, not the document's" in
      assert(blocks("> # H\n").forall {
        case _: CstNode.AtxHeading => false
        case _                     => true
      })
  }
