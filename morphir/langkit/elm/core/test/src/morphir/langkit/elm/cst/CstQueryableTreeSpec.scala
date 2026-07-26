package morphir.langkit.elm.cst

import parsley.{Failure, Success}
import kyo.test.*

import morphir.langkit.elm.Krueger
import morphir.langkit.elm.Span
import morphir.langkit.elm.cst.CstQueryableTree.given
import morphir.langkit.trees.CaptureName
import morphir.langkit.trees.FieldName
import morphir.langkit.trees.NodeTypeName
import morphir.langkit.trees.QueryableTree
import morphir.langkit.trees.query.*

class CstQueryableTreeSpec extends Test[Any]:

  private def parse(src: String): CstModule = Krueger.parseCst(src) match
    case Success(m)   => m
    case Failure(msg) => throw new AssertionError(s"parse failed: $msg")

  private val source =
    """module App exposing (..)
      |
      |import Html
      |
      |main = 42
      |""".stripMargin

  private val moduleTree: CstModule      = parse(source)
  private val root: CstNode              = moduleTree
  private val qt: QueryableTree[CstNode] = summon[QueryableTree[CstNode]]

  private def typeNameOf(n: CstNode): String = NodeTypeName.unwrap(qt.nodeType(n))

  private def field(s: String): FieldName = FieldName.make(s).toOption.get
  private def cap(s: String): CaptureName = CaptureName.make(s).toOption.get

  "QueryableTree[CstNode]" - {
    "nodeType" - {
      "uses simple class name for concrete variants" in {
        assert(typeNameOf(moduleTree) == "CstModule")
        assert(typeNameOf(moduleTree.moduleDecl) == "CstModuleDeclaration")
        assert(typeNameOf(moduleTree.moduleDecl.name) == "CstQualifiedName")
      }
      "distinguishes value declarations from other declaration kinds" in {
        val valueDecls = moduleTree.declarations.collect { case v: CstValueDeclaration => v }
        assert(valueDecls.size == 1)
        assert(valueDecls.head.name.value == "main")
        assert(valueDecls.forall(v => typeNameOf(v) == "CstValueDeclaration"))
      }
    }
    "children" - {
      "match CstVisitor.children for every node in the parsed module" in {
        val mismatches = CstVisitor.collect(moduleTree) {
          case n if qt.children(n) != CstVisitor.children(n) => n
        }
        assert(mismatches.isEmpty)
      }
    }
    "fields" - {
      "CstValueDeclaration exposes name, body, patterns, annotation" in {
        val valueDecl = moduleTree.declarations
          .collectFirst { case v: CstValueDeclaration =>
            v
          }
          .getOrElse(throw new AssertionError("no value declaration"))
        val fs = qt.fields(valueDecl)
        assert(fs.keySet == Set(field("annotation"), field("name"), field("patterns"), field("body")))
        assert(fs(field("name")) == Seq(valueDecl.name))
        assert(fs(field("body")) == Seq(valueDecl.body))
        assert(fs(field("patterns")) == valueDecl.patterns.toSeq)
        assert(fs(field("annotation")) == valueDecl.annotation.toSeq)
      }
      "CstModule exposes moduleDecl, imports, declarations" in {
        val fs = qt.fields(moduleTree)
        assert(fs.keySet.contains(field("moduleDecl")))
        assert(fs.keySet.contains(field("imports")))
        assert(fs.keySet.contains(field("declarations")))
        assert(fs(field("moduleDecl")) == Seq(moduleTree.moduleDecl))
        assert(fs(field("imports")) == moduleTree.imports.toSeq)
        assert(fs(field("declarations")) == moduleTree.declarations.toSeq)
      }
      "CstName has no fields" in {
        val name = moduleTree.moduleDecl.name.parts.head
        assert(qt.fields(name).isEmpty)
      }
    }
    "text" - {
      "CstName returns its value" in {
        val name = moduleTree.moduleDecl.name.parts.head
        assert(qt.text(name).contains(name.value))
      }
      "CstIntLiteral stringifies its value" in {
        val intLit = CstIntLiteral(42L)(Span.zero)
        assert(qt.text(intLit).contains("42"))
      }
      "CstStringLiteral returns its raw string" in {
        val strLit = CstStringLiteral("hi")(Span.zero)
        assert(qt.text(strLit).contains("hi"))
      }
      "compound nodes return None" in {
        assert(qt.text(moduleTree).isEmpty)
        assert(qt.text(moduleTree.moduleDecl).isEmpty)
      }
    }
    "integration with Matcher" - {
      "a node-pattern query surfaces every value declaration" in {
        val query = QueryParser.parse("(CstValueDeclaration name: (CstName) @n)") match
          case Success(q) => q
          case Failure(e) => throw new AssertionError(s"bad query: $e")
        val ms    = Matcher.matches(query, root).toList
        val names = ms.flatMap(_.captures.get(cap("n"))).collect { case n: CstName => n.value }
        assert(names.toSet == Set("main"))
      }
    }
  }
