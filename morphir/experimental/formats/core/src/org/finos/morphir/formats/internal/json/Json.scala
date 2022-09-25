package org.finos.morphir.formats.internal.json

final case class JsonFolder[A](str: String => A, num: Int => A, obj: Map[String, A] => A)
