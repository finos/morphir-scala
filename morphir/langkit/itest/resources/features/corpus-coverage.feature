Feature: Conformance corpus coverage
  The conformance corpus is the suite's evidence that a construct still parses.
  It is held to a written list of CST node types, so a construct that stops
  being produced — or was never produced — fails here rather than going quiet.

  Scenario: Every corpus module parses
    When every conformance corpus module is parsed
    Then every required CST node type is exercised
