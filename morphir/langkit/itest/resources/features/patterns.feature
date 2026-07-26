Feature: Patterns
  Krueger parses patterns used in function parameter lists.

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
