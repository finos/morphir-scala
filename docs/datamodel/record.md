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

and the equivalent Morphir/ELM value:
```elm
person: { name: String, age: Int }
person = { name = "Joe", age = 123 }
```

The Data and Concept that represents the above is as follows:
```scala
Data.Record(
  values = List(
    L("name") -> Data.String("Joe"),
    L("age") -> Data.Int(123)
  )
  shape = Concept.Record(
    values = List(
      L("name") -> Concept.String
      L("age") -> Concept.Int
    )
  )
)
```

The fields of records may themselves be records (as well as collections, enums, or any other kind of Data object).
