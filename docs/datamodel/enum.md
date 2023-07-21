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
and the equivalent in Morphir/ELM:
```elm
-- Types
type alias OneStreamSink = { topic: String }
type Sink =
  OneStream {- sinkData: -} OneStreamSink
  | ConsoleLog

-- Values:
os: Sink
os = OneStream { topic = "123" }

oc: Sink
oc = ConsoleLog
```

The value `os` would be represented in the Morphir data-model as the following:
