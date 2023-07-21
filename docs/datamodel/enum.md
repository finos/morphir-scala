---
id: enums
title: Enum (Discriminated Union) Encoding
---
Given the following Scala types and values:
```scala
// Types:
case class OneStreamSink(topic: String)
sealed trait Sink
object Sink {
  case class OneStream(sinkData: OneStreamSink) extends Sink
  case object ConsoleLog extends Sink
}

// Values:
val os = Sink.OneStream(OneStreamSink(topic = "123"))
val oc = Sink.ConsoleLog
```
