package org.finos.morphir
package ir
package json
import zio.json._
import zio.json.ast.Json
import org.finos.morphir.ir.module.ModuleName

trait NamingJsonEncoders extends JsonEncodingHelpers {
  implicit val nameEncoder: JsonEncoder[Name] = JsonEncoder.list[String].contramap(name => name.toList)
  implicit val pathEncoder: JsonEncoder[Path] = JsonEncoder.list[Name].contramap(path => path.segments.toList)
  implicit val modulePathEncoder: JsonEncoder[ModulePath]   = pathEncoder.contramap(_.toPath)
  implicit val packageNameEncoder: JsonEncoder[PackageName] = pathEncoder.contramap(_.toPath)
  implicit val qNameEncoder: JsonEncoder[QName] =
    Json.encoder.contramap[QName](qName =>
      Json.Arr(toJsonAstOrThrow(qName.modulePath), toJsonAstOrThrow(qName.localName))
    )

  implicit val fqNameEncoder: JsonEncoder[FQName] =
    Json.encoder.contramap[FQName](fqName =>
      Json.Arr(
        toJsonAstOrThrow(fqName.packagePath),
        toJsonAstOrThrow(fqName.modulePath),
        toJsonAstOrThrow(fqName.localName)
      )
    )

  implicit val moduleNameEncoder: JsonEncoder[ModuleName] =
    Json.encoder.contramap[ModuleName](moduleName =>
      Json.Arr(toJsonAstOrThrow(moduleName.namespace), toJsonAstOrThrow(moduleName.localName))
    )
}


