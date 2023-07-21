---
id: Maps
title: Maps
---
Given the following Scala Map (and Morphir/ELM dictionary)
```scala// Scala
val myMap = Map("foo" -> 123, "bar" -> 456)
```
```elm
-- Morphir/ELM
myMap: Dict String Int
myMap = Dict.fromList
    [
        ("foo", 123),
        ("bar", 456)
    ]
```
