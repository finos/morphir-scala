Feature: CST to AST lowering
  CstLowering normalises the tree by stripping syntactic noise.

  Scenario: Lowering flattens the qualified module name
    Given the Elm source:
      """
      module Data.List exposing (..)
      """
    When the source is parsed to an AST
    Then the AST module is named "Data.List"

  Scenario: Lowering propagates imports into the AST
    Given the Elm source:
      """
      module M exposing (..)

      import Http.Body
      """
    When the source is parsed to an AST
    Then the AST has 1 import
    And AST import 1 is from module "Http.Body"

  Scenario: Lowering captures value declaration names
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the source is parsed to an AST
    Then AST declaration 1 is a value named "main"

  Scenario: Lowering surfaces type annotations
    Given the Elm source:
      """
      module M exposing (..)

      foo : Int
      foo = 42
      """
    When the source is parsed to an AST
    Then AST declaration 1 is a value named "foo"
    And AST value "foo" has a type annotation

  Scenario: Lowering keeps a plain module plain
    Given the Elm source:
      """
      module M exposing (..)

      x = 1
      """
    When the source is parsed to an AST
    Then the AST module is plain
    And the AST module has no effect manager

  Scenario: Lowering carries the port module header
    Given the Elm source:
      """
      port module Ports exposing (..)

      x = 1
      """
    When the source is parsed to an AST
    Then the AST module is port
    And the AST module has no effect manager

  Scenario: Lowering carries the effect module manager
    Given the Elm source:
      """
      effect module Eff where { command = MyCmd, subscription = MySub } exposing (..)

      x = 1
      """
    When the source is parsed to an AST
    Then the AST module is effect
    And the AST effect manager command is "MyCmd"
    And the AST effect manager subscription is "MySub"
