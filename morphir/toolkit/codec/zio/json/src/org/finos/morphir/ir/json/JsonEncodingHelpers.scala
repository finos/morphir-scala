package org.finos.morphir.ir.json

import zio.json._
import zio.json.ast.Json

trait JsonEncodingHelpers {
  final private[json] def toJsonAstOrThrow[A](a: A)(implicit encoder: JsonEncoder[A]): Json =
    a.toJsonAST.toOption.get
}
