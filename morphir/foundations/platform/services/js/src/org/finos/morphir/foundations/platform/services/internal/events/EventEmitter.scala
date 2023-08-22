package org.finos.morphir.foundations.platform.services.internal.events

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.annotation.nowarn

@js.native
@JSImport("events", "EventEmitter")
class EventEmitter() extends IEventEmitter {
  def this(options: EventEmitterOptions) = this()
}

@js.native
trait IEventEmitter extends js.Object {
  @nowarn var Domain: Domain = js.native

  def addListener(eventName: String, listener: js.Function): this.type = js.native
  def emit(name: String, args: js.Any*): Boolean                       = js.native
  def eventNames(): js.Array[String]                                   = js.native

  def listenerCount(eventName: String): Int                     = js.native
  def listeners(eventName: String): js.Array[js.Function]       = js.native
  def off(eventName: String, listner: js.Function): this.type   = js.native
  def on(eventName: String, listener: js.Function): this.type   = js.native
  def once(eventName: String, listener: js.Function): this.type = js.native
}

trait EventEmitterOptions extends js.Object {
  var captureRejections: Boolean
}

object EventEmitterOptions {
  def apply(
      captureRejections: Boolean
  ): EventEmitterOptions = {
    val _obj$ = js.Dynamic.literal(
      "captureRejections" -> captureRejections.asInstanceOf[js.Any]
    )
    _obj$.asInstanceOf[EventEmitterOptions]
  }
}

@js.native
@JSImport("events", JSImport.Namespace)
object EventEmitter extends js.Object {

  @nowarn var usingDomains: Boolean = js.native
}

@js.native
trait Domain extends js.Object
