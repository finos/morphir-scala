---
id: maps
title: Maps
---
Given the following Scala Map:
```scala// Scala
val myMap = Map("foo" -> 123, "bar" -> 456)
```
and Morphir/ELM dictionary:
```elm
-- Morphir/ELM
myMap: Dict String Int
myMap = Dict.fromList
    [
        ("foo", 123),
        ("bar", 456)
    ]
```

This data should be represented in the Morphir data-model as the following:
```scala
Data.Map(
    values = Map(
        Data.String("foo") -> Data.Int(123),
        Data.String("bar") -> Data.Int(456),
    )
    shape = Concept.Map(keyType = Concept,String, valueType = Concept.Int)
)
```
