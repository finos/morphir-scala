Feature: Tree queries
  Krueger ships a tree-sitter-inspired query DSL that selects
  CST or AST nodes by pattern, with captures and predicates.

  # EARS requirement map:
  # - REQ-QRY-001 (When): When a supported query feature is used, the system shall produce expected captures/matches.
  # - REQ-QRY-002 (If): If query text is invalid or semantically invalid, then the system shall fail with assertable diagnostics.
  # - REQ-QRY-003 (Where): Where cross-tree behavior is intended, the system shall assert equivalent/expected CST and AST outcomes.
  # - REQ-QRY-004 (Where): Where ordering is observable, the system shall preserve deterministic match ordering.

  @REQ-QRY-001
  Scenario: CST query surfaces a single value declaration
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstValueDeclaration name: (CstName) @n)"
    Then the query matches exactly 1 time
    And capture "n" of match 1 is a "CstName"
    And capture "n" of match 1 has text "main"

  @REQ-QRY-001
  Scenario: CST wildcard matches every node in the tree
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "_"
    Then the query matches at least 5 times

  @REQ-QRY-001
  Scenario: CST query with a predicate filters captures
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName) @n (#eq? @n \"main\")"
    Then the query matches exactly 1 time
    And capture "n" of match 1 has text "main"

  @REQ-QRY-001
  Scenario: CST query with a regex predicate
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName) @n (#match? @n \"^m\")"
    Then the query matches at least 1 time

  @REQ-QRY-001
  Scenario: CST query with no matches yields an empty result
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstFloatLiteral)"
    Then the query has no matches

  @REQ-QRY-001
  Scenario: AST query surfaces the value declaration by name
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the AST is queried with "(ValueDeclaration) @v"
    Then the query matches exactly 1 time
    And capture "v" of match 1 is a "ValueDeclaration"
    And capture "v" of match 1 has text "main"

  @REQ-QRY-001
  Scenario: AST query with a predicate on the captured text
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the AST is queried with "(ValueDeclaration) @v (#eq? @v \"main\")"
    Then the query matches exactly 1 time

  @REQ-QRY-002
  Scenario: Predicate capture must be bound
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName) @n (#eq? @missing \"main\")"
    Then the query fails with message containing "@missing"

  @REQ-QRY-001 @REQ-QRY-004
  Scenario: CST query supports ordered unfielded child patterns
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstValueDeclaration (CstName) @n (CstIntLiteral) @i)"
    Then the query matches exactly 1 time
    And capture "n" of match 1 has text "main"
    And capture "i" of match 1 has text "42"

  @REQ-QRY-001 @REQ-QRY-004
  Scenario: CST query supports anchored adjacent child patterns
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstValueDeclaration (CstName) @n . (CstIntLiteral) @i)"
    Then the query matches exactly 1 time
    And capture "n" of match 1 has text "main"
    And capture "i" of match 1 has text "42"

  @REQ-QRY-001
  Scenario: Negated field constraint matches when optional field is absent
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstValueDeclaration !annotation name: (CstName) @n)"
    Then the query matches exactly 1 time
    And capture "n" of match 1 has text "main"

  @REQ-QRY-001 @REQ-QRY-004
  Scenario: Alternation query matches either branch deterministically
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "[(CstName) @n (CstIntLiteral) @i]"
    Then capture "n" of match 1 has text "M"
    And capture "n" of match 2 has text "main"
    And capture "i" of match 3 has text "42"

  @REQ-QRY-001 @REQ-QRY-004
  Scenario: Child quantifiers support optional and zero-or-more matching
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstValueDeclaration (CstName) @n? (CstComment)*)"
    Then the query matches exactly 1 time
    And capture "n" of match 1 has text "main"

  @REQ-QRY-001 @REQ-QRY-004
  Scenario: CST query with multiple top-level patterns
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName) (CstIntLiteral)"
    Then the query matches at least 2 times

  @REQ-QRY-001
  Scenario: Multi-pattern query can include predicates
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName) @n (CstIntLiteral) (#eq? @n \"main\")"
    Then the query matches at least 1 time

  @REQ-QRY-002
  Scenario: Unknown capture in multi-pattern query fails query parse
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName) @n (CstIntLiteral) (#eq? @missing \"x\")"
    Then the query fails with message containing "@missing"

  @REQ-QRY-002
  Scenario: Malformed query with unmatched paren fails with stable parse prefix
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName"
    Then the query fails with message containing "Query parse failed:"

  @REQ-QRY-002
  Scenario: Malformed predicate arity fails with stable parse prefix
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName) @n (#eq? @n)"
    Then the query fails with message containing "Query parse failed:"

  @REQ-QRY-002
  Scenario: Predicate argument kind mismatch fails query parse
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName) @n (#match? \"main\" \"^m\")"
    Then the query fails with message containing "Query parse failed:"

  @REQ-QRY-002
  Scenario: Unknown predicate fails with explicit predicate diagnostic
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName) @n (#foo? @n \"main\")"
    Then the query fails with message containing "#foo?"

  @REQ-QRY-001
  Scenario: CST query with #not-eq? excludes matching text
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstValueDeclaration name: (CstName) @n) (#not-eq? @n \"main\")"
    Then the query has no matches

  @REQ-QRY-002
  Scenario: Unsupported directive fails with explicit directive diagnostic
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName) @n (#set! @n \"kind\")"
    Then the query fails with message containing "Unsupported directive"

  @REQ-QRY-002
  Scenario: Invalid anchor placement fails with explicit diagnostic
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstValueDeclaration . (CstName) @n (CstIntLiteral) @i)"
    Then the query fails with message containing "invalid anchor placement"

  @REQ-QRY-002
  Scenario: Conflicting negated and positive field constraints fail parse
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstValueDeclaration !name name: (CstName) @n)"
    Then the query fails with message containing "conflicting field constraints"

  @REQ-QRY-002
  Scenario: Empty alternation fails with explicit diagnostic
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "[]"
    Then the query fails with message containing "alternation requires at least one branch"

  @REQ-QRY-002
  Scenario: Invalid quantifier placement fails with explicit diagnostic
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstValueDeclaration ? (CstName) @n)"
    Then the query fails with message containing "invalid quantifier placement"

  @REQ-QRY-002
  Scenario: Duplicate capture names fail query parse
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName) @n (CstIntLiteral) @n"
    Then the query fails with message containing "duplicate capture"

  @REQ-QRY-003
  Scenario: Parity baseline - CST and AST produce same match count for declaration query
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstValueDeclaration name: (CstName) @n)"
    And the query match count is remembered as "cstDeclCount"
    When the AST is queried with "(ValueDeclaration) @v"
    Then the query match count equals remembered "cstDeclCount"

  @REQ-QRY-003
  Scenario: Parity negative - CST and AST both return zero matches for absent type query
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstFloatLiteral)"
    And the query match count is remembered as "cstZeroCount"
    When the AST is queried with "(FloatLiteral)"
    Then the query match count equals remembered "cstZeroCount"
    And the query has no matches

  @REQ-QRY-002 @REQ-QRY-003
  Scenario: Parity failure - malformed query fails for both CST and AST paths
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName"
    Then the query fails with message containing "Query parse failed:"
    When the AST is queried with "(ValueDeclaration"
    Then the query fails with message containing "Query parse failed:"

  @REQ-QRY-004
  Scenario: Ordered capture assertions report deterministic match ordering
    Given the Elm source:
      """
      module M exposing (..)

      alpha = 1
      beta = 2
      """
    When the AST is queried with "(ValueDeclaration) @v"
    Then capture "v" texts in match order are:
      """
      alpha
      beta
      """

  @REQ-QRY-002
  Scenario: Failure phase assertion distinguishes query parse failures
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the AST is queried with "(ValueDeclaration"
    Then the query fails during "query-parse"

  @REQ-QRY-002
  Scenario: Failure phase assertion distinguishes tree parse failures
    Given the Elm source:
      """
      module M exposing (..)

      main =
      """
    When the AST is queried with "(ValueDeclaration) @v"
    Then the query fails during "tree-parse"

  @REQ-QRY-002
  Scenario: Failure message hook can remember and compare diagnostics
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstName"
    Then the query failure message is remembered as "parseDiag"
    When the CST is queried with "(CstName"
    Then the query failure message equals remembered "parseDiag"

  @REQ-QRY-001 @REQ-QRY-004
  Scenario: Regression - ordered child mismatch yields no CST match
    Given the Elm source:
      """
      module M exposing (..)

      main = 42
      """
    When the CST is queried with "(CstValueDeclaration (CstIntLiteral) @i (CstName) @n)"
    Then the query has no matches

  @REQ-QRY-001 @REQ-QRY-004
  Scenario: Regression - multi-pattern grouping preserves first-pattern ordering
    Given the Elm source:
      """
      module M exposing (..)

      alpha = 1
      beta = 2
      """
    When the AST is queried with "(ValueDeclaration) @v (IntLiteral) @i"
    Then capture "v" of match 1 has text "alpha"
    And capture "v" of match 2 has text "beta"
    And capture "i" of match 3 has text "1"
    And capture "i" of match 4 has text "2"

  @REQ-QRY-001 @REQ-QRY-003 @REQ-QRY-004
  Scenario: Canonical pretty-print roundtrips query semantics
    Given the query source:
      """
      ( CstValueDeclaration   name: (CstName) @n  !typeAnnotation )
      ( #not-eq?   @n   "tmp" )
      """
    When the query is canonicalized
    Then the canonical query text is:
      """
      (CstValueDeclaration name: (CstName) @n !typeAnnotation)
      (#not-eq? @n "tmp")
      """
    And the canonical query reparses successfully
