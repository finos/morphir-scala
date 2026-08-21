package morphir.langkit.markdown.cst

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.HeadingLevel

/**
 * That graduated blocks come back typed, and ungraduated regions stay verbatim.
 *
 * The round-trip suite proves nothing is lost; this suite proves something is gained — a parse where every node were
 * verbatim would round-trip perfectly and mean nothing. One test per graduated form. Block structure is fully typed
 * now; what stays verbatim is interior prose, which is the inline slices' problem (lc8.24 through lc8.26).
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

  "graduated containers" - {

    "a block quote holds its marker as a token and its content typed" in {
      blocks("> quoted\n").head match
        case CstNode.BlockQuote(children, _) =>
          assert(children.headOption.exists {
            case CstNode.Token("> ", _) => true
            case _                      => false
          })
          assert(children.exists {
            case _: CstNode.Paragraph => true
            case _                    => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }

    "a two-line quote's interior marker is a token inside the paragraph" in {
      blocks("> a\n> b\n").head match
        case CstNode.BlockQuote(children, _) =>
          val para = children.collect { case p: CstNode.Paragraph => p }
          assert(para.size == 1)
          assert(para.head.childNodes.exists {
            case CstNode.Token("> ", _) => true
            case _                      => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }

    "a heading inside a quote is typed inside the quote" in {
      blocks("> # H\n").head match
        case CstNode.BlockQuote(children, _) =>
          assert(children.exists {
            case CstNode.AtxHeading(HeadingLevel.One, _, _) => true
            case _                                          => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }

    "a nested quote is a quote inside a quote" in {
      blocks("> > deep\n").head match
        case CstNode.BlockQuote(children, _) =>
          assert(children.exists {
            case _: CstNode.BlockQuote => true
            case _                     => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }

    "a bullet list carries its bullet and its item's marker token" in {
      blocks("- item\n").head match
        case CstNode.BulletList('-', tight, children, _) =>
          assert(tight)
          children.head match
            case CstNode.ListItem(itemChildren, _) =>
              assert(itemChildren.headOption.exists {
                case CstNode.Token("- ", _) => true
                case _                      => false
              })
              assert(itemChildren.exists {
                case _: CstNode.Paragraph => true
                case _                    => false
              })
            case other => assert(false, s"expected ListItem first, got $other")
        case other => assert(false, s"expected BulletList, got $other")
    }

    "an ordered list keeps its start number and delimiter" in {
      blocks("3) go\n4) stop\n").head match
        case CstNode.OrderedList(start, delimiter, tight, children, _) =>
          assert(start == 3)
          assert(delimiter == ')')
          assert(tight)
          assert(children.collect { case i: CstNode.ListItem => i }.size == 2)
        case other => assert(false, s"expected OrderedList, got $other")
    }

    "a blank line between items makes the list loose" in {
      blocks("- a\n\n- b\n").head match
        case CstNode.BulletList(_, tight, _, _) => assert(!tight)
        case other                              => assert(false, s"expected BulletList, got $other")
    }

    "a continuation line's indentation is a token inside the item's paragraph" in {
      blocks("- a\n  b\n").head match
        case CstNode.BulletList(_, _, children, _) =>
          val para = children.head.childNodes.collect { case p: CstNode.Paragraph => p }
          assert(para.size == 1)
          assert(para.head.childNodes.exists {
            case CstNode.Token("  ", _) => true
            case _                      => false
          })
        case other => assert(false, s"expected BulletList, got $other")
    }

    "a list inside a quote nests, marker bytes shared but owned once" in {
      blocks("> - a\n").head match
        case CstNode.BlockQuote(children, _) =>
          assert(children.exists {
            case _: CstNode.BulletList => true
            case _                     => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }

    "an empty item is its marker token alone" in {
      blocks("-\n").head match
        case CstNode.BulletList(_, _, children, _) =>
          children.head match
            case CstNode.ListItem(itemChildren, _) =>
              assert(itemChildren.forall {
                case _: CstNode.Token => true
                case _                => false
              })
            case other => assert(false, s"expected ListItem, got $other")
        case other => assert(false, s"expected BulletList, got $other")
    }
  }

  "html and definitions" - {

    "an HTML block is typed with a text interior" in {
      blocks("<div>\nhi\n</div>\n").head match
        case CstNode.HtmlBlock(children, _) =>
          assert(children.forall {
            case _: CstNode.Text => true
            case _               => false
          })
        case other => assert(false, s"expected HtmlBlock, got $other")
    }

    "a link reference definition is its own node" in {
      blocks("[a]: /url\n").head match
        case CstNode.LinkReferenceDefinition(_, _) => assert(true)
        case other                                 => assert(false, s"expected LinkReferenceDefinition, got $other")
    }

    "a definition before a paragraph leaves the paragraph typed" in {
      val nodes = blocks("[a]: /url\nrest\n")
      assert(nodes.headOption.exists {
        case _: CstNode.LinkReferenceDefinition => true
        case _                                  => false
      })
      assert(nodes.exists {
        case _: CstNode.Paragraph => true
        case _                    => false
      })
    }

    "a definition inside a quote nests" in {
      blocks("> [a]: /url\n").head match
        case CstNode.BlockQuote(children, _) =>
          assert(children.exists {
            case _: CstNode.LinkReferenceDefinition => true
            case _                                  => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }

    "a reference paragraph stays a paragraph, resolution being lowering's job" in {
      blocks("[a]\n\n[a]: /u\n").toSeq match
        case Seq(CstNode.Paragraph(_, _), _, CstNode.LinkReferenceDefinition(_, _), _*) => assert(true)
        case other => assert(false, s"expected paragraph then definition, got $other")
    }
  }
