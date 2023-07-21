---
id: records
title: Record (Case Class) Encoding
---
Scala Case Classes and Morphir/ELM records are represented as the Data.Record type.
Given the following Scala values:
```scala
case class Person(name: String, age: Int)
val joe = Person("Joe", 123)
```
