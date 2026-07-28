package morphir.langkit.elm.ast

import parsley.{Failure, Success}
import kyo.test.*

import morphir.langkit.elm.Elm
import morphir.langkit.core.Span
import morphir.langkit.elm.ast.AstQueryableTree.given
import morphir.langkit.trees.CaptureName
import morphir.langkit.trees.FieldName
import morphir.langkit.trees.NodeTypeName
import morphir.langkit.trees.QueryableTree
import morphir.langkit.trees.query.*

class AstQueryableTreeSpec extends Test[Any]:

  private def parse(src: String): Module = Elm.parseAst(src) match
    case Success(m)   => m
    case Failure(msg) => throw new AssertionError(s"parse failed: $msg")

  private val source =
    """module App exposing (..)
      |
      |import Html
      |
      |main = 42
      |""".stripMargin

  private val effectSource =
    """effect module Eff where { command = MyCmd, subscription = MySub } exposing (..)
      |
      |x = 1
      |""".stripMargin

  private val moduleTree: Module         = parse(source)
  private val root: AstNode              = moduleTree
  private val qt: QueryableTree[AstNode] = summon[QueryableTree[AstNode]]

  private def typeNameOf(n: AstNode): String = NodeTypeName.unwrap(qt.nodeType(n))

  private def field(s: String): FieldName = FieldName.make(s).toOption.get
  private def cap(s: String): CaptureName = CaptureName.make(s).toOption.get

  "QueryableTree[AstNode]" - {
    "nodeType" - {
      "uses simple class name for concrete variants" in {
        assert(typeNameOf(moduleTree) == "Module")
        assert(typeNameOf(moduleTree.name) == "QualifiedName")
      }
    }
    "children" - {
      "match AstVisitor.children for every node in the parsed module" in {
        val mismatches = AstVisitor.collect(moduleTree) {
          case n if qt.children(n) != AstVisitor.children(n) => n
        }
        assert(mismatches.isEmpty)
      }
    }
    "fields" - {
      "ValueDeclaration exposes typeAnnotation, parameters, body" in {
        val valueDecl = moduleTree.declarations
          .collectFirst { case v: ValueDeclaration => v }
          .getOrElse(throw new AssertionError("no value declaration"))
        val fs = qt.fields(valueDecl)
        assert(fs.keySet == Set(field("typeAnnotation"), field("parameters"), field("body")))
        assert(fs(field("body")) == Seq(valueDecl.body))
        assert(fs(field("parameters")) == valueDecl.parameters.toSeq)
        assert(fs(field("typeAnnotation")) == valueDecl.typeAnnotation.toSeq)
      }
      "Module exposes exposing, manager, imports, declarations as fields" in {
        val fs = qt.fields(moduleTree)
        assert(fs.keySet == Set(field("exposing"), field("manager"), field("imports"), field("declarations")))
        assert(fs(field("exposing")) == Seq(moduleTree.exposing))
        assert(fs(field("manager")) == Seq.empty)
        assert(fs(field("imports")) == moduleTree.imports.toSeq)
        assert(fs(field("declarations")) == moduleTree.declarations.toSeq)
      }
      "an effect module's manager is both a field and a child" in {
        val effect  = parse(effectSource)
        val manager = effect.manager.getOrElse(throw new AssertionError("no effect manager"))
        assert(typeNameOf(manager) == "EffectManager")
        assert(qt.fields(effect)(field("manager")) == Seq(manager))
        assert(qt.children(effect).contains(manager))
        assert(qt.fields(manager).isEmpty)
        assert(qt.children(manager).isEmpty)
      }
    }
    "text" - {
      "IntLiteral stringifies its value" in {
        val lit = IntLiteral(7L)(Span.zero)
        assert(qt.text(lit).contains("7"))
      }
      "StringLiteral returns its raw string" in {
        val lit = StringLiteral("hello")(Span.zero)
        assert(qt.text(lit).contains("hello"))
      }
      "QualifiedName returns its dotted full name" in
        assert(qt.text(moduleTree.name).contains("App"))
      "ValueDeclaration surfaces its name" in {
        val valueDecl = moduleTree.declarations
          .collectFirst { case v: ValueDeclaration => v }
          .getOrElse(throw new AssertionError("no value declaration"))
        assert(qt.text(valueDecl).contains("main"))
      }
    }
    "integration with Matcher" - {
      "a node-pattern query surfaces each value declaration" in {
        val query = QueryParser.parse("(ValueDeclaration) @v") match
          case Success(q) => q
          case Failure(e) => throw new AssertionError(s"bad query: $e")
        val ms    = Matcher.matches(query, root).toList
        val names = ms.flatMap(_.captures.get(cap("v"))).collect { case v: ValueDeclaration => v.name }
        assert(names.toSet == Set("main"))
      }
    }
  }
