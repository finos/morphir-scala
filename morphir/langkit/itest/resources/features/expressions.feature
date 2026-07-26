Feature: Expressions
  Krueger parses Elm expressions appearing as declaration bodies.
  Assertions use the generic query DSL instead of hand-rolled step verbs.

  Scenario: Integer literal body
    Given the Elm source:
      """
      module M exposing (..)

      x = 42
      """
    When the CST is queried with:
      """
      (CstValueDeclaration name: (CstName) @n body: (CstIntLiteral) @b)
      (#eq? @n "x")
      (#eq? @b "42")
      """
    Then the query matches exactly 1 time

  Scenario: Float literal body
    Given the Elm source:
      """
      module M exposing (..)

      pi = 3.14
      """
    When the CST is queried with:
      """
      (CstValueDeclaration name: (CstName) @n body: (CstFloatLiteral) @b)
      (#eq? @n "pi")
      """
    Then the query matches exactly 1 time

  Scenario: Unit literal body
    Given the Elm source:
      """
      module M exposing (..)

      u = ()
      """
    When the CST is queried with:
      """
      (CstValueDeclaration name: (CstName) @n body: (CstUnitLiteral) @b)
      (#eq? @n "u")
      """
    Then the query matches exactly 1 time

  Scenario: List literal body
    Given the Elm source:
      """
      module M exposing (..)

      xs = [1, 2, 3]
      """
    When the CST is queried with:
      """
      (CstValueDeclaration name: (CstName) @n body: (CstListLiteral) @b)
      (#eq? @n "xs")
      """
    Then the query matches exactly 1 time
    And capture "b" of match 1 has 3 direct children

  Scenario: Record literal body
    Given the Elm source:
      """
      module M exposing (..)

      rec = { x = 1, y = 2 }
      """
    When the CST is queried with:
      """
      (CstValueDeclaration name: (CstName) @n body: (CstRecordLiteral) @b)
      (#eq? @n "rec")
      """
    Then the query matches exactly 1 time
    And capture "b" of match 1 has 2 direct children
