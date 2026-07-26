Feature: Module parsing
  Krueger parses Elm module headers, qualified module names, and imports.

  Scenario: Parses a minimal plain module
    Given the Elm source:
      """
      module Main exposing (..)
      """
    When the source is parsed
    Then the module is named "Main"
    And the module is plain

  Scenario: Parses a port module
    Given the Elm source:
      """
      port module Ports exposing (..)
      """
    When the source is parsed
    Then the module is named "Ports"
    And the module is port

  Scenario: Parses an effect module
    Given the Elm source:
      """
      effect module Eff exposing (..)
      """
    When the source is parsed
    Then the module is named "Eff"
    And the module is effect

  Scenario: Parses a qualified module name
    Given the Elm source:
      """
      module Data.List exposing (..)
      """
    When the source is parsed
    Then the module is named "Data.List"

  Scenario: Parses a plain import
    Given the Elm source:
      """
      module M exposing (..)

      import List
      """
    When the source is parsed
    Then the module has 1 import
    And import 1 is from module "List"

  Scenario: Parses an aliased import
    Given the Elm source:
      """
      module M exposing (..)

      import Data.List as L
      """
    When the source is parsed
    Then import 1 is aliased as "L"

  Scenario: Parses an import with an exposing list
    Given the Elm source:
      """
      module M exposing (..)

      import Html exposing (text, div)
      """
    When the source is parsed
    Then import 1 exposes values "text,div"
