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

An instance of this record would have the following Morphir data-model encoding.
```scala
// Instance
val joe = Person("Joe", 123)

// Encoding
Data.Record(
    values = List(
        Label("name") -> Data.String("Joe"),
        Label("age") -> Data.Int32(123)
    )
)
```
