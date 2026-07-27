# morphir-dataformats

Provides various dataformats which are provided by Morphir.

## Attribution

The data formats here take some inspiration from Smithy and Amazon's Ion formats, as well as obvious formats like JSON and S-Expressions.

We make use of some of the concepts found in [Smithy4s](https://disneystreaming.github.io/smithy4s/) in this codebase. Smithy4s is [licensed](https://github.com/disneystreaming/smithy4s/blob/63cf8eba47356bb155bf4354354ab737f0d5718c/LICENSE) under the Tomorrow Open Source Technology License. 

## TODOs

- [ ] Rename module to morphir-dataformats
- [ ] Add Concept and Schema[A]
  - Concept is similar to Schema but is not a GADT
- Add DynamicValue, AmorphousValue, or FlexValue to represent unconstrained data without schema or Concept