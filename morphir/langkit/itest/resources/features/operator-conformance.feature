Feature: Operator conformance
  Binary operators are grouped by the precedence and associativity Elm gives them,
  and chains Elm refuses to group are refused rather than guessed at. Departures
  from Elm are opt-in through parse options.

  Scenario: A non-associative operator cannot be chained
    Given the Elm source:
      """
      module M exposing (..)

      main = a == b == c
      """
    When the source is parsed
    Then the parse fails with code "ELM-P004"
    And the parse failure message contains "You cannot mix (==) and (==) without parentheses."

  Scenario: Operators of equal precedence leaning opposite ways cannot be mixed
    Given the Elm source:
      """
      module M exposing (..)

      main = a |> f <| g
      """
    When the source is parsed
    Then the parse fails with code "ELM-P004"
    And the parse failure message contains "You cannot mix (|>) and (<|) without parentheses."

  Scenario: Parentheses resolve a conflicting chain
    Given the Elm source:
      """
      module M exposing (..)

      main = (a == b) == c
      """
    When the source is parsed
    Then the module is named "M"

  Scenario: An operator no fixity in scope declares is rejected
    Given the Elm source:
      """
      module M exposing (..)

      main = a <%> b
      """
    When the source is parsed
    Then the parse fails with code "ELM-P005"
    And the parse failure message contains "I do not know the precedence or associativity of (<%>)."

  Scenario: An infix declaration in the module supplies the missing fixity
    Given the Elm source:
      """
      module M exposing (..)

      infix right 5 (<%>) = combine

      main = a <%> b <%> c
      """
    When the CST is queried with:
      """
      (CstValueDeclaration body: (CstBinaryOp operator: (CstName) @op right: (CstBinaryOp)))
      (#eq? @op "<%>")
      """
    Then the query matches exactly 1 time

  Scenario: Operators bundled from official packages are known
    Given the Elm source:
      """
      module M exposing (..)

      route = a </> b |= c
      """
    When the source is parsed
    Then the module is named "M"

  Scenario: Lenient options accept a chain Elm refuses, grouping it to the left
    Given the "lenient" parse options
    And the Elm source:
      """
      module M exposing (..)

      main = a == b == c
      """
    When the CST is queried with:
      """
      (CstValueDeclaration body: (CstBinaryOp left: (CstBinaryOp operator: (CstName) @inner) operator: (CstName) @op))
      (#eq? @op "==")
      (#eq? @inner "==")
      """
    Then the query matches exactly 1 time

  Scenario: Lenient options accept an operator of unknown fixity
    Given the "lenient" parse options
    And the Elm source:
      """
      module M exposing (..)

      main = a <%> b
      """
    When the source is parsed
    Then the module is named "M"

  Scenario: Reserved sequences are not binary operators
    Given the Elm source:
      """
      module M exposing (..)

      main = a | b
      """
    When the source is parsed
    Then the parse fails with code "ELM-P002"
