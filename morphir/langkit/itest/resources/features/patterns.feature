Feature: Patterns
  The Elm langkit parses patterns used in function parameter lists.

  Scenario: Variable pattern
    Given the Elm source:
      """
      module M exposing (..)

      f x = 42
      """
    When the source is parsed
    Then declaration 1 is a value named "f"
    And value "f" has 1 parameter

  Scenario: Wildcard pattern
    Given the Elm source:
      """
      module M exposing (..)

      f _ = 42
      """
    When the source is parsed
    Then declaration 1 is a value named "f"
    And value "f" has 1 parameter

  Scenario: Constructor pattern with no arguments
    Given the Elm source:
      """
      module M exposing (..)

      f Nothing = 0
      """
    When the source is parsed
    Then declaration 1 is a value named "f"
    And value "f" has 1 parameter

  Scenario: Multiple parameters
    Given the Elm source:
      """
      module M exposing (..)

      add x y = 0
      """
    When the source is parsed
    Then value "add" has 2 parameters

  Scenario: Cons pattern in a case branch
    Given the Elm source:
      """
      module M exposing (..)

      main = case xs of
          x :: rest -> x
          [] -> 0
      """
    When the CST is queried with "(CstConsPattern head: (CstVariablePattern) @h)"
    Then the query matches exactly 1 time
    And capture "h" of match 1 is a "CstVariablePattern"

  Scenario: Cons patterns nest to the right
    Given the Elm source:
      """
      module M exposing (..)

      main = case xs of
          a :: b :: rest -> a
          [] -> 0
      """
    When the CST is queried with "(CstConsPattern tail: (CstConsPattern) @inner)"
    Then the query matches exactly 1 time
    And capture "inner" of match 1 is a "CstConsPattern"
