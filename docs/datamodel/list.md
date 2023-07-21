---
id: Lists
title: Lists
---
Given the following Scala and Morphir/ELM lists:
```scala
// Scala
val items = List("one", "two", "three")
```

```elm
-- Morphir/ELM
items: List String
items = ["one", "two", "three"]
```

This data should be represented in the Morphir data-model as the following:
```scala
Data.List(
    values = List(Data.String("one"), Data.String("two"), Data.String("three")),
    shape = Concept.List(elementType = Concept.String)
)
```

List should be able to contain Records, Enums, or any other subtype of Data.
For example, the following data in Scala and Morphir/Elm:
