Feature: Comment parsing

  Scenario: Distinguishes doc comments from regular comments
    Given the Elm source:
      """
      module App exposing (..)

      -- regular line
      {- regular block -}
      {-| module docs -}
      main = "-- not a comment"
      """
    When the source is parsed
    Then the module has 2 comments
    And comment 1 is a "line" comment with text "regular line"
    And comment 2 is a "block" comment with text "regular block"
    And declaration 1 has doc comment "module docs"

  Scenario: Module doc comment is associated with the module
    Given the Elm source:
      """
      module App exposing (..)

      {-| This is the App module -}

      import Html

      main = 42
      """
    When the source is parsed
    Then the module doc comment is "This is the App module"
    And declaration 1 has no doc comment

  Scenario: Multiple declarations with doc comments
    Given the Elm source:
      """
      module App exposing (..)

      {-| A record alias -}
      type alias Model = { count : Int }

      {-| A custom type -}
      type Color = Red | Blue
      """
    When the source is parsed
    Then the module has no doc comment
    And declaration 1 has doc comment "A record alias"
    And declaration 2 has doc comment "A custom type"

  Scenario: No doc comments leaves trivia empty
    Given the Elm source:
      """
      module App exposing (..)

      -- just a line comment
      type alias Name = String
      """
    When the source is parsed
    Then the module has 1 comment
    And comment 1 is a "line" comment with text "just a line comment"
    And the module has no doc comment
    And declaration 1 has no doc comment
