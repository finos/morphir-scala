package org.finos.morphir.foundations.platform.services.internal.process

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
trait Process extends js.Object {
    /// What processor architecture you're running on: 'arm', 'arm64', 'ia32', or 'x64'.
    def arch:String = js.native

    /// Returns the current 
    def cwd(): String = js.native
}

@js.native
@JSGlobal("process")
object Process extends Process {}
