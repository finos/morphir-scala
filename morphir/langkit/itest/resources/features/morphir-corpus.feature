Feature: Morphir corpus parser fixtures
  Krueger parses representative Elm modules sourced from finos/morphir-examples and finos/morphir-elm.

  Scenario Outline: Parse a Morphir corpus fixture
    Given the Elm fixture "<fixture>"
    When the source is parsed
    Then the module is named "<module>"
    And the module has <declarations> declarations

    Examples:
      | fixture                                                                                                                              | module                                      | declarations |
      | fixtures/morphir/finos/morphir-examples/tutorial/step_1_first_logic/src/Morphir/Example/App/Rentals.elm                              | Morphir.Example.App.Rentals                | 1            |
      | fixtures/morphir/finos/morphir-examples/tutorial/step_2_business_language/src/Morphir/Example/App/BusinessTerms.elm                   | Morphir.Example.App.BusinessTerms          | 12           |
      | fixtures/morphir/finos/morphir-examples/tutorial/step_2_business_language/src/Morphir/Example/App/Forecast.elm                        | Morphir.Example.App.Forecast               | 6            |
      | fixtures/morphir/finos/morphir-examples/tutorial/step_2_business_language/src/Morphir/Example/App/Analytics.elm                       | Morphir.Example.App.Analytics              | 1            |
      | fixtures/morphir/finos/morphir-elm/src/Morphir/File/Path.elm                                                                           | Morphir.File.Path                          | 1            |
      | fixtures/morphir/finos/morphir-elm/tests-integration/typespec/model/src/TestModel/BasicTypes.elm                                       | TestModel.BasicTypes                       | 6            |
      | fixtures/morphir/finos/morphir-elm/tests-integration/json-schema/model/src/TestModel/TupleTypes.elm                                    | TestModel.TupleTypes                       | 2            |
      | fixtures/morphir/finos/morphir-elm/tests-integration/reference-model/src/Morphir/Reference/Model/Issues/Issue350.elm                  | Morphir.Reference.Model.Issues.Issue350    | 1            |
