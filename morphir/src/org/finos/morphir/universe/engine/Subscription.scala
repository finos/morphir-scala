package org.finos.morphir.universe.engine

import org.finos.morphir.datamodel.DataEncoder
import zio.prelude.*

final case class Subscription[A: DataEncoder](topic: Subscription.Topic)
object Subscription {

  type Topic = Topic.Type
  object Topic extends Subtype[List[String]] {
    implicit val TopicDebug: Debug[Topic] = Debug.make { case topic =>
      Debug.Repr.String(topic.mkString("/"))
    }
  }
}
