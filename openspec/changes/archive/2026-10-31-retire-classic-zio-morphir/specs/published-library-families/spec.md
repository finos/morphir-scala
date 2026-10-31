# Spec Delta

## Purpose

States which library families morphir-scala publishes, the effect stack a published library may depend on, and how
a published library family is retired without surprising its consumers.

## ADDED Requirements

### Requirement: Published libraries are Kyo-based
Every library morphir-scala publishes SHALL be free of ZIO on its runtime classpath, on every platform it is
published for, once the classic family has been removed.

#### Scenario: No ZIO in any published runtime classpath
- **WHEN** the runtime classpath of every module selected for publication is resolved
- **THEN** none contains a `dev.zio` artifact

#### Scenario: Knowledge libraries do not reach the classic root
- **WHEN** the dependencies of `morphir-knowledge-logic` and `morphir-knowledge-okf` are resolved
- **THEN** neither includes the classic `morphir` library or ZIO

### Requirement: A family is deprecated before it is removed
A published library family SHALL carry a deprecation notice in at least one released milestone before the milestone
that stops publishing it.

#### Scenario: Deprecation milestone
- **WHEN** the milestone that deprecates the classic family is released
- **THEN** its CHANGELOG has a `Deprecated` entry naming each classic artifact and the Kyo module that replaces it,
  and the classic artifacts are still published in that milestone

#### Scenario: Removal milestone
- **WHEN** the milestone that removes the classic family is released
- **THEN** its CHANGELOG has a `Removed` entry naming each artifact that is no longer published, and the milestone
  is labelled breaking

### Requirement: Deprecation is visible to consumers of the code
Each deprecated published module SHALL tell a consumer, in its scaladoc entry points and module README, that it is
deprecated and which module replaces it, or that it has no replacement.

#### Scenario: Module with a replacement
- **WHEN** a consumer reads the entry-point scaladoc of a deprecated module that has a Kyo replacement
- **THEN** it names the replacement module and its artifact

#### Scenario: Module without a replacement
- **WHEN** a consumer reads the entry-point scaladoc of `morphir-runtime-classic`
- **THEN** it states that Scala evaluation of Morphir IR has no replacement yet

### Requirement: Released artifacts stay available
Retiring a family SHALL stop new releases only; it SHALL NOT remove or alter versions already published.

#### Scenario: After removal
- **WHEN** the classic family has been removed
- **THEN** every classic artifact version published before removal still resolves from Maven Central
