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

  private def blocks(source: String): Chunk[MdcCstNode] = CstParser.parse(source).children

  private def tokenTexts(node: MdcCstNode): Chunk[String] =
    node.childNodes.collect { case MdcCstNode.Token(text, _) => text }

  "graduated blocks" - {

    "a thematic break is a typed node holding one token" in {
      blocks("***\n") match
        case Chunk(MdcCstNode.ThematicBreak(_, _), MdcCstNode.Verbatim("\n", _)) => assert(true)
        case other => assert(false, s"expected ThematicBreak + newline gap, got $other")
    }

    "an ATX heading keeps its level and its marker token" in {
      blocks("## Hi\n").head match
        case MdcCstNode.AtxHeading(level, _, _) =>
          assert(level == HeadingLevel.Two)
          assert(tokenTexts(blocks("## Hi\n").head).exists(_.startsWith("##")))
        case other => assert(false, s"expected AtxHeading, got $other")
    }

    "an ATX heading keeps its closing sequence as a token" in {
      blocks("# Hi #\n").head match
        case MdcCstNode.AtxHeading(_, _, _) =>
          assert(tokenTexts(blocks("# Hi #\n").head).size == 2)
        case other => assert(false, s"expected AtxHeading, got $other")
    }

    "a setext heading keeps its underline as a token" in {
      blocks("Hi\n===\n").head match
        case MdcCstNode.SetextHeading(level, _, _) =>
          assert(level == HeadingLevel.One)
          assert(tokenTexts(blocks("Hi\n===\n").head).exists(_.contains("===")))
        case other => assert(false, s"expected SetextHeading, got $other")
    }

    "a closed fence is token, text, token" in {
      blocks("```scala\ncode\n```\n").head match
        case MdcCstNode.FencedCode(children, _) =>
          children.toSeq match
            case Seq(MdcCstNode.Token(open, _), MdcCstNode.Text(_, _), MdcCstNode.Token(close, _)) =>
              assert(open.startsWith("```"))
              assert(close.startsWith("```"))
            case other => assert(false, s"expected token/text/token, got $other")
        case other => assert(false, s"expected FencedCode, got $other")
    }

    "an unterminated fence has no closing token" in {
      blocks("```\ncode\n").head match
        case MdcCstNode.FencedCode(children, _) =>
          assert(children.collect { case t: MdcCstNode.Token => t }.size == 1)
        case other => assert(false, s"expected FencedCode, got $other")
    }

    "indented code is typed, interior still verbatim" in {
      blocks("    code\n").head match
        case MdcCstNode.IndentedCode(_, _) => assert(true)
        case other                         => assert(false, s"expected IndentedCode, got $other")
    }

    "a paragraph is typed, interior still verbatim until inlines graduate" in {
      blocks("plain prose\n").head match
        case MdcCstNode.Paragraph(_, _) => assert(true)
        case other                      => assert(false, s"expected Paragraph, got $other")
    }
  }

  "graduated containers" - {

    "a block quote holds its marker as a token and its content typed" in {
      blocks("> quoted\n").head match
        case MdcCstNode.BlockQuote(children, _) =>
          assert(children.headOption.exists {
            case MdcCstNode.Token("> ", _) => true
            case _                         => false
          })
          assert(children.exists {
            case _: MdcCstNode.Paragraph => true
            case _                       => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }

    "a two-line quote's interior marker is a token inside the paragraph" in {
      blocks("> a\n> b\n").head match
        case MdcCstNode.BlockQuote(children, _) =>
          val para = children.collect { case p: MdcCstNode.Paragraph => p }
          assert(para.size == 1)
          assert(para.head.childNodes.exists {
            case MdcCstNode.Token("> ", _) => true
            case _                         => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }

    "a heading inside a quote is typed inside the quote" in {
      blocks("> # H\n").head match
        case MdcCstNode.BlockQuote(children, _) =>
          assert(children.exists {
            case MdcCstNode.AtxHeading(HeadingLevel.One, _, _) => true
            case _                                             => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }

    "a nested quote is a quote inside a quote" in {
      blocks("> > deep\n").head match
        case MdcCstNode.BlockQuote(children, _) =>
          assert(children.exists {
            case _: MdcCstNode.BlockQuote => true
            case _                        => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }

    "a bullet list carries its bullet and its item's marker token" in {
      blocks("- item\n").head match
        case MdcCstNode.BulletList('-', tight, children, _) =>
          assert(tight)
          children.head match
            case MdcCstNode.ListItem(itemChildren, _) =>
              assert(itemChildren.headOption.exists {
                case MdcCstNode.Token("- ", _) => true
                case _                         => false
              })
              assert(itemChildren.exists {
                case _: MdcCstNode.Paragraph => true
                case _                       => false
              })
            case other => assert(false, s"expected ListItem first, got $other")
        case other => assert(false, s"expected BulletList, got $other")
    }

    "an ordered list keeps its start number and delimiter" in {
      blocks("3) go\n4) stop\n").head match
        case MdcCstNode.OrderedList(start, delimiter, tight, children, _) =>
          assert(start == 3)
          assert(delimiter == ')')
          assert(tight)
          assert(children.collect { case i: MdcCstNode.ListItem => i }.size == 2)
        case other => assert(false, s"expected OrderedList, got $other")
    }

    "a blank line between items makes the list loose" in {
      blocks("- a\n\n- b\n").head match
        case MdcCstNode.BulletList(_, tight, _, _) => assert(!tight)
        case other                                 => assert(false, s"expected BulletList, got $other")
    }

    "a continuation line's indentation is a token inside the item's paragraph" in {
      blocks("- a\n  b\n").head match
        case MdcCstNode.BulletList(_, _, children, _) =>
          val para = children.head.childNodes.collect { case p: MdcCstNode.Paragraph => p }
          assert(para.size == 1)
          assert(para.head.childNodes.exists {
            case MdcCstNode.Token("  ", _) => true
            case _                         => false
          })
        case other => assert(false, s"expected BulletList, got $other")
    }

    "a list inside a quote nests, marker bytes shared but owned once" in {
      blocks("> - a\n").head match
        case MdcCstNode.BlockQuote(children, _) =>
          assert(children.exists {
            case _: MdcCstNode.BulletList => true
            case _                        => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }

    "an empty item is its marker token alone" in {
      blocks("-\n").head match
        case MdcCstNode.BulletList(_, _, children, _) =>
          children.head match
            case MdcCstNode.ListItem(itemChildren, _) =>
              assert(itemChildren.forall {
                case _: MdcCstNode.Token => true
                case _                   => false
              })
            case other => assert(false, s"expected ListItem, got $other")
        case other => assert(false, s"expected BulletList, got $other")
    }
  }

  "html and definitions" - {

    "an HTML block is typed with a text interior" in {
      blocks("<div>\nhi\n</div>\n").head match
        case MdcCstNode.HtmlBlock(children, _) =>
          assert(children.forall {
            case _: MdcCstNode.Text => true
            case _                  => false
          })
        case other => assert(false, s"expected HtmlBlock, got $other")
    }

    "a link reference definition is its own node" in {
      blocks("[a]: /url\n").head match
        case MdcCstNode.LinkReferenceDefinition(_, _) => assert(true)
        case other                                    => assert(false, s"expected LinkReferenceDefinition, got $other")
    }

    "a definition before a paragraph leaves the paragraph typed" in {
      val nodes = blocks("[a]: /url\nrest\n")
      assert(nodes.headOption.exists {
        case _: MdcCstNode.LinkReferenceDefinition => true
        case _                                     => false
      })
      assert(nodes.exists {
        case _: MdcCstNode.Paragraph => true
        case _                       => false
      })
    }

    "a definition inside a quote nests" in {
      blocks("> [a]: /url\n").head match
        case MdcCstNode.BlockQuote(children, _) =>
          assert(children.exists {
            case _: MdcCstNode.LinkReferenceDefinition => true
            case _                                     => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }

    "a reference paragraph stays a paragraph, resolution being lowering's job" in {
      blocks("[a]\n\n[a]: /u\n").toSeq match
        case Seq(MdcCstNode.Paragraph(_, _), _, MdcCstNode.LinkReferenceDefinition(_, _), _*) => assert(true)
        case other => assert(false, s"expected paragraph then definition, got $other")
    }
  }

  "graduated inlines" - {

    def paragraphChildren(source: String): Chunk[MdcCstNode] =
      blocks(source).head match
        case MdcCstNode.Paragraph(children, _) => children
        case other                             => throw new AssertionError(s"expected Paragraph, got $other")

    "a code span is backtick tokens around raw text" in {
      paragraphChildren("before ``code`` after\n").collect { case c: MdcCstNode.CodeSpan => c } match
        case Chunk(code) =>
          code.childNodes.toSeq match
            case Seq(MdcCstNode.Token("``", _), MdcCstNode.Text("code", _), MdcCstNode.Token("``", _)) => assert(true)
            case other => assert(false, s"expected token/text/token, got $other")
        case other => assert(false, s"expected one CodeSpan, got $other")
    }

    "an autolink keeps its angle brackets as tokens" in {
      paragraphChildren("see <https://example.com> now\n").collect { case a: MdcCstNode.Autolink => a } match
        case Chunk(link) =>
          link.childNodes.toSeq match
            case Seq(MdcCstNode.Token("<", _), MdcCstNode.Text("https://example.com", _), MdcCstNode.Token(">", _)) =>
              assert(true)
            case other => assert(false, s"expected bracketed autolink, got $other")
        case other => assert(other.size == 1, s"expected one Autolink, got $other")
    }

    "inline raw HTML is typed with its verbatim value" in
      assert(paragraphChildren("a <b x='y'> c\n").exists {
        case _: MdcCstNode.RawHtml => true
        case _                     => false
      })

    "a code span inside emphasis is typed inside the emphasis" in {
      paragraphChildren("*a `b` c*\n").collectFirst { case e: MdcCstNode.Emphasis => e } match
        case Some(emphasis) =>
          assert(emphasis.childNodes.exists {
            case _: MdcCstNode.CodeSpan => true
            case _                      => false
          })
        case None => assert(false, "expected an Emphasis node")
    }

    "emphasis keeps its delimiter and its run tokens" in {
      paragraphChildren("a *b* c\n").collectFirst { case e: MdcCstNode.Emphasis => e } match
        case Some(MdcCstNode.Emphasis(delimiter, strong, children, _)) =>
          assert(delimiter == '*')
          assert(!strong)
          assert(children.collect { case MdcCstNode.Token(t, _) => t }.toSeq == Seq("*", "*"))
        case _ => assert(false, "expected an Emphasis node")
    }

    "strong emphasis spends two delimiters a side" in {
      paragraphChildren("__b__\n").collectFirst { case e: MdcCstNode.Emphasis => e } match
        case Some(MdcCstNode.Emphasis(delimiter, strong, children, _)) =>
          assert(delimiter == '_')
          assert(strong)
          assert(children.collect { case MdcCstNode.Token(t, _) => t }.toSeq == Seq("__", "__"))
        case _ => assert(false, "expected a strong Emphasis node")
    }

    "a partially consumed run leaves its extra delimiter verbatim" in {
      paragraphChildren("*foo**\n").collectFirst { case e: MdcCstNode.Emphasis => e } match
        case Some(MdcCstNode.Emphasis(_, strong, children, _)) =>
          assert(!strong)
          assert(children.collect { case MdcCstNode.Token(t, _) => t }.toSeq == Seq("*", "*"))
        case _ => assert(false, "expected an Emphasis node")
    }

    "a hard break keeps its spelling" in
      assert(paragraphChildren("a\\\nb\n").exists {
        case MdcCstNode.HardBreak(children, _) =>
          children.collect { case MdcCstNode.Token(t, _) => t }.mkString.startsWith("\\")
        case _ => false
      })

    "an escape is a backslash token and the literal character" in {
      paragraphChildren("a \\* b\n").collectFirst { case e: MdcCstNode.Escape => e } match
        case Some(escape) =>
          escape.childNodes.toSeq match
            case Seq(MdcCstNode.Token("\\", _), MdcCstNode.Text("*", _)) => assert(true)
            case other => assert(false, s"expected backslash token + char, got $other")
        case None => assert(false, "expected an Escape node")
    }

    "an entity keeps its raw spelling" in {
      paragraphChildren("a &amp; b\n").collectFirst { case e: MdcCstNode.Entity => e } match
        case Some(entity) =>
          assert(entity.childNodes.toSeq match
            case Seq(MdcCstNode.Token("&amp;", _)) => true
            case _                                 => false)
        case None => assert(false, "expected an Entity node")
    }

    "a heading's code span is typed inside the heading" in {
      blocks("# uses `f`\n").head match
        case MdcCstNode.AtxHeading(_, children, _) =>
          assert(children.exists {
            case _: MdcCstNode.CodeSpan => true
            case _                      => false
          })
        case other => assert(false, s"expected AtxHeading, got $other")
    }

    "a multi-line code span in a quote keeps the interior marker as a token" in {
      blocks("> a `b\n> c` d\n").head match
        case MdcCstNode.BlockQuote(children, _) =>
          val spans = children.flatMap(_.childNodes).collect { case c: MdcCstNode.CodeSpan => c }
          assert(spans.size == 1)
          assert(spans.head.childNodes.exists {
            case MdcCstNode.Token("> ", _) => true
            case _                         => false
          })
        case other => assert(false, s"expected BlockQuote, got $other")
    }
  }

  "graduated links and images" - {

    def firstLink(source: String): MdcCstNode.Link =
      blocks(source).head.childNodes.collectFirst { case l: MdcCstNode.Link => l } match
        case Some(link) => link
        case None       => throw new AssertionError(s"no Link in ${blocks(source).head}")

    "an inline link keeps its tokens, destination and title spelling" in {
      val link = firstLink("[text](/url \"title\")\n")
      assert(link.form == LinkForm.Inline)
      val tokens = link.childNodes.collect { case MdcCstNode.Token(t, _) => t }
      assert(tokens.toSeq == Seq("[", "](", "\"", "\"", ")"))
      val texts = link.childNodes.collect { case MdcCstNode.Text(t, _) => t }
      assert(texts.toSeq == Seq("/url", "title"))
    }

    "an angle-bracketed destination keeps its brackets as tokens" in {
      val link = firstLink("[t](</my url>)\n")
      assert(link.childNodes.collect { case MdcCstNode.Text(t, _) => t }.toSeq == Seq("/my url"))
      assert(link.childNodes.collect { case MdcCstNode.Token(t, _) => t }.contains("<"))
    }

    "a full reference link keeps its second label" in {
      val link = firstLink("[text][ref]\n\n[ref]: /url\n")
      assert(link.form == LinkForm.ReferenceFull)
      assert(link.childNodes.collect { case MdcCstNode.Text(t, _) => t }.toSeq == Seq("ref"))
    }

    "a collapsed reference link is bracketed as written" in {
      val link = firstLink("[ref][]\n\n[ref]: /url\n")
      assert(link.form == LinkForm.ReferenceCollapsed)
      assert(link.childNodes.collect { case MdcCstNode.Token(t, _) => t }.toSeq == Seq("[", "][]"))
    }

    "a shortcut reference link is just its brackets" in {
      val link = firstLink("[ref]\n\n[ref]: /url\n")
      assert(link.form == LinkForm.ReferenceShortcut)
      assert(link.childNodes.collect { case MdcCstNode.Token(t, _) => t }.toSeq == Seq("[", "]"))
    }

    "a link's text is inline content, code spans typed inside it" in {
      val link = firstLink("[a `b` c](/u)\n")
      assert(link.childNodes.exists {
        case _: MdcCstNode.CodeSpan => true
        case _                      => false
      })
    }

    "an image's alt is inline content in the CST" in {
      blocks("![alt `x`](/img)\n").head.childNodes.collectFirst { case i: MdcCstNode.Image => i } match
        case Some(image) =>
          assert(image.form == LinkForm.Inline)
          assert(image.childNodes.exists {
            case _: MdcCstNode.CodeSpan => true
            case _                      => false
          })
        case None => assert(false, s"no Image in ${blocks("![alt `x`](/img)\n").head}")
    }

    "an unresolved shortcut stays verbatim, not a link" in
      assert(blocks("[nope]\n").head.childNodes.forall {
        case _: MdcCstNode.Verbatim => true
        case _                      => false
      })
  }
