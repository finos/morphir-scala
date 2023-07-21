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

Note that the equivalent representation of this data in ELM is the following:
```elm
person: { name: String, age: Int }
person = { name = "Joe", age = 123 }
```

Every single instance of the data-model has a Data.shape property that represents the schema. The schema of this 
record will look like the following:

```scala
Concept.Record(
    values = List(
        Label("name") -> Concept.String
        Label("age") -> Concept.Int
    )
)
```

With this in mind, complete data available on the Data.Record instance above can be thought of as the following. 
(NOTE: the shorthand L("name") will be used for Label("name") now on).
