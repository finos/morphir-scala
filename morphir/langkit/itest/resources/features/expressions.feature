Feature: Expressions
  The Elm langkit parses Elm expressions appearing as declaration bodies.
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

  Scenario: Multiplication binds tighter than addition
    Given the Elm source:
      """
      module M exposing (..)

      main = 1 + 2 * 3
      """
    When the CST is queried with:
      """
      (CstValueDeclaration body: (CstBinaryOp operator: (CstName) @op right: (CstBinaryOp operator: (CstName) @inner)))
      (#eq? @op "+")
      (#eq? @inner "*")
      """
    Then the query matches exactly 1 time

  Scenario: Left-associative operators at equal precedence group to the left
    Given the Elm source:
      """
      module M exposing (..)

      main = 1 - 2 - 3
      """
    When the CST is queried with:
      """
      (CstValueDeclaration body: (CstBinaryOp left: (CstBinaryOp operator: (CstName) @inner) operator: (CstName) @op))
      (#eq? @op "-")
      (#eq? @inner "-")
      """
    Then the query matches exactly 1 time

  Scenario: Cons associates to the right
    Given the Elm source:
      """
      module M exposing (..)

      main = a :: b :: rest
      """
    When the CST is queried with:
      """
      (CstValueDeclaration body: (CstBinaryOp operator: (CstName) @op right: (CstBinaryOp operator: (CstName) @inner)))
      (#eq? @op "::")
      (#eq? @inner "::")
      """
    Then the query matches exactly 1 time

  Scenario: A module's own infix declaration decides its operator's associativity
    Given the Elm source:
      """
      module M exposing (..)

      infix right 5 (<%>) = combine

      main = a <%> b <%> c
      """
    When the CST is queried with:
      """
      (CstValueDeclaration body: (CstBinaryOp operator: (CstName) @op right: (CstBinaryOp operator: (CstName) @inner)))
      (#eq? @op "<%>")
      (#eq? @inner "<%>")
      """
    Then the query matches exactly 1 time

  Scenario: The lowered AST keeps the precedence-shaped tree
    Given the Elm source:
      """
      module M exposing (..)

      main = 1 + 2 * 3
      """
    When the AST is queried with:
      """
      (ValueDeclaration body: (BinaryOp right: (BinaryOp) @inner) @outer)
      (#eq? @outer "+")
      (#eq? @inner "*")
      """
    Then the query matches exactly 1 time

  Scenario: Let binding without a type annotation
    Given the Elm source:
      """
      module M exposing (..)

      main =
          let
              y = 1
          in
          y
      """
    When the CST is queried with:
      """
      (CstValueDeclaration body: (CstLetIn (CstLetBinding pattern: (CstVariablePattern (CstName) @b))))
      (#eq? @b "y")
      """
    Then the query matches exactly 1 time

  Scenario: Let binding with a type annotation
    Given the Elm source:
      """
      module M exposing (..)

      main =
          let
              y : Int
              y = 1
          in
          y
      """
    When the CST is queried with:
      """
      (CstValueDeclaration body: (CstLetIn (CstLetBinding annotation: (CstTypeAnnotation name: (CstName) @a))))
      (#eq? @a "y")
      """
    Then the query matches exactly 1 time
