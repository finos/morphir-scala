package morphir.langkit.elm.parser

import parsley.{Failure, Success}
import kyo.test.*

import morphir.langkit.elm.cst.*

class ModuleParserSpec extends Test[Any]:

  private def parseOrFail(src: String): CstModule =
    ModuleParser.module.parse(src) match
      case Success(m)   => m
      case Failure(msg) => throw new AssertionError(s"parse failed: $msg\nSource:\n$src")

  "ModuleParser" - {
    "parses minimal plain module" in {
      val m = parseOrFail("module Main exposing (..)\n")
      assert(m.moduleDecl.moduleType == ModuleType.Plain)
      assert(m.moduleDecl.name.parts.map(_.value) == List("Main"))
      assert(m.moduleDecl.exposing.isInstanceOf[CstExposingAll])
    }
    "parses port module" in {
      val m = parseOrFail("port module Ports exposing (..)\n")
      assert(m.moduleDecl.moduleType == ModuleType.Port)
    }
    "parses effect module" in {
      val m = parseOrFail("effect module Eff exposing (..)\n")
      assert(m.moduleDecl.moduleType == ModuleType.Effect)
    }
    "parses qualified module name" in {
      val m = parseOrFail("module Data.List exposing (..)\n")
      assert(m.moduleDecl.name.parts.map(_.value) == List("Data", "List"))
    }
    "parses plain import" in {
      val m = parseOrFail("module M exposing (..)\nimport List\n")
      assert(m.imports.map(_.moduleName.parts.map(_.value)) == List(List("List")))
    }
    "parses import with alias" in {
      val m = parseOrFail("module M exposing (..)\nimport Data.List as L\n")
      assert(m.imports.head.alias.map(_.value).contains("L"))
      assert(m.imports.head.moduleName.parts.map(_.value) == List("Data", "List"))
    }
    "parses import with exposing list" in {
      val m     = parseOrFail("module M exposing (..)\nimport Html exposing (text, div)\n")
      val items = m.imports.head.exposing match
        case Some(e: CstExposingExplicit) => e.items.collect { case v: CstExposedValue => v.name.value }
        case _                            => Nil
      assert(items == List("text", "div"))
    }
    "parses annotated value declaration without consuming the value name as a type argument" in {
      val m    = parseOrFail("module M exposing (..)\nfoo : Int\nfoo = 42\n")
      val decl = m.declarations.head match
        case v: CstValueDeclaration => v
        case other                  => throw new AssertionError(s"expected value declaration, got $other")
      assert(decl.name.value == "foo")
      assert(decl.annotation.exists(_.name.value == "foo"))
      assert(decl.annotation.exists(_.typeExpr.isInstanceOf[CstTypeReference]))
      assert(decl.body == CstIntLiteral(42L)(decl.body.span))
    }
    "parses lower-case value references and record field access" in {
      val m    = parseOrFail("module M exposing (..)\nfoo : { bar : Int } -> Int\nfoo record = record.bar\n")
      val decl = m.declarations.head match
        case v: CstValueDeclaration => v
        case other                  => throw new AssertionError(s"expected value declaration, got $other")
      val access = decl.body match
        case a: CstFieldAccess => a
        case other             => throw new AssertionError(s"expected field access, got $other")
      assert(access.field.value == "bar")
      assert(access.record.asInstanceOf[CstVariableRef].name.parts.map(_.value) == List("record"))
    }
    "fails on malformed module header" in {
      ModuleParser.module.parse("module !!!") match
        case Failure(_) => succeed
        case Success(_) => throw new AssertionError("expected failure")
    }
  }
