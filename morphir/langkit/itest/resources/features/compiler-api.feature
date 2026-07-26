Feature: Compiler API backends
  The compiler API exposes a canonical invoke contract that supported
  backends must preserve.

  # EARS requirement map:
  # - REQ-CAPI-001 (When): When a supported backend receives a valid compiler request, the system shall return a successful compiler response.
  # - REQ-CAPI-002 (If): If a supported backend receives malformed Elm source, then the system shall return structured compiler errors.
  # - REQ-CAPI-003 (Where): Where a compiler request is repeated on the same backend, the system shall return deterministic JSON responses.
  # - REQ-CAPI-004 (Where): Where the canonical compiler API is invoked through JVM, Chicory, or Scala.js-on-Node, the system shall preserve equivalent response semantics.

  @REQ-CAPI-001 @REQ-CAPI-004
  Scenario Outline: Supported compiler backends parse valid Elm source
    Given the compiler backend "<backend>"
    And the compiler input:
      """
      {"source":"module Demo exposing (..)\n\nmain = 42\n"}
      """
    When compiler operation "parseCst" is invoked
    Then the compiler response is ok
    And the compiler response value contains "CstModule("

    Examples:
      | backend |
      | jvm           |
      | chicory       |

  @REQ-CAPI-002 @REQ-CAPI-004
  Scenario Outline: Supported compiler backends return structured parse errors
    Given the compiler backend "<backend>"
    And the compiler input:
      """
      {"source":"module Demo exposing (..)\n\nmain =\n"}
      """
    When compiler operation "parseCst" is invoked
    Then the compiler response is not ok
    And the compiler response has at least 1 error
    And compiler error 1 phase is "cst"
    And compiler error 1 message contains "I ran into the end of the file unexpectedly"

    Examples:
      | backend |
      | jvm           |
      | chicory       |

  @REQ-CAPI-003 @REQ-CAPI-004
  Scenario Outline: Supported compiler backends are deterministic for repeated requests
    Given the compiler backend "<backend>"
    And the compiler input:
      """
      {"source":"module Demo exposing (..)\n\nmain = 42\n"}
      """
    When compiler operation "parseCst" is invoked twice
    Then the compiler responses are byte-identical
    And the compiler response is ok

    Examples:
      | backend |
      | jvm           |
      | chicory       |
