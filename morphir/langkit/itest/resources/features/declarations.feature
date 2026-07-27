Feature: Declarations
  The Elm langkit parses top-level value, type alias, and custom type declarations.

  Scenario: Value declaration without annotation
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the source is parsed
    Then the module has 1 declaration
    And declaration 1 is a value named "main"

  Scenario: Value declaration with annotation
    Given the Elm source:
      """
      module M exposing (..)

      foo : Int
      foo = 42
      """
    When the source is parsed
    Then the module has 1 declaration
    And declaration 1 is a value named "foo"

  Scenario: Type alias declaration
    Given the Elm source:
      """
      module M exposing (..)

      type alias User = { id : Int }
      """
    When the source is parsed
    Then the module has 1 declaration
    And declaration 1 is a type alias named "User"

  Scenario: Custom type declaration
    Given the Elm source:
      """
      module M exposing (..)

      type Maybe a = Just a | Nothing
      """
    When the source is parsed
    Then the module has 1 declaration
    And declaration 1 is a custom type named "Maybe"
    And custom type "Maybe" has 2 constructors

  Scenario: Multi-argument type annotations nest to the right
    Given the Elm source:
      """
      module M exposing (..)

      f : a -> b -> c
      f = 1
      """
    When the CST is queried with "(CstFunctionType to: (CstFunctionType) @inner)"
    Then the query matches exactly 1 time
    And capture "inner" of match 1 is a "CstFunctionType"
