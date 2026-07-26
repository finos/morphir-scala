Feature: Declarations
  Krueger parses top-level value, type alias, and custom type declarations.

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
