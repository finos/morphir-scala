---
id: datamodel
title: Data Model
---

The data model represents a list of data-types represented by the Mapper data-model specification. For every
data-type in the hosted-value list, an encoding exists to that data-type from a simple Scala type (also known as the
Scala -> Morphir DDL encoding).

For every data-type in the data model, a decoding will also be implemented from the morphir evaluator’s output
ResultValue type (also known as the Morphir-IR -> Morphir DDL decoding).
