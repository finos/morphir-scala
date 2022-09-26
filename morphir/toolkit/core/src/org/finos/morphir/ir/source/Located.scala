package org.finos.morphir.ir.source

final case class Located[+A](at: Region, value: A)
