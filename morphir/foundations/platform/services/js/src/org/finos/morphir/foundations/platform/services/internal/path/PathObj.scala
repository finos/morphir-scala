package org.finos.morphir.foundations.platform.services.internal.path

import scala.scalajs.js 

trait PathObj extends js.Object {
  var root: js.UndefOr[String] = js.undefined
  var dir: js.UndefOr[String]  = js.undefined
  var base: js.UndefOr[String] = js.undefined
  var ext: js.UndefOr[String]  = js.undefined
  var name: js.UndefOr[String] = js.undefined
}

object PathObj {
  def apply(
      root: js.UndefOr[String] = js.undefined,
      dir: js.UndefOr[String] = js.undefined,
      base: js.UndefOr[String] = js.undefined,
      ext: js.UndefOr[String] = js.undefined,
      name: js.UndefOr[String] = js.undefined
  ): PathObj = {
    val _obj$ = js.Dynamic.literal(
    )
    root.foreach(_v => _obj$.updateDynamic("root")(_v.asInstanceOf[js.Any]))
    dir.foreach(_v => _obj$.updateDynamic("dir")(_v.asInstanceOf[js.Any]))
    base.foreach(_v => _obj$.updateDynamic("base")(_v.asInstanceOf[js.Any]))
    ext.foreach(_v => _obj$.updateDynamic("ext")(_v.asInstanceOf[js.Any]))
    name.foreach(_v => _obj$.updateDynamic("name")(_v.asInstanceOf[js.Any]))
    _obj$.asInstanceOf[PathObj]
  }
}
