---
id: schema
title: Data and Schema
---

Every data-instance in the Morphir data-model is a subtype of the Morphir Data interface. The instances contain their 
corresponding data as well as a Data.shape property that represents the schema of the data. For example, for the
following simple record type:
```scala
case class Person(name: String, age: Int)
```
