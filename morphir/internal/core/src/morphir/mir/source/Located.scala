package org.finos.morphir.mir.source

final case class Located[+A](at: Region, value: A)
