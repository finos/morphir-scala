package org.finos.morphir.samples

import org.finos.morphir.ir.json._
import org.finos.morphir.ir._

import zio._
import zio.json._
import zio.json.ast.Json

import org.finos.morphir.ir.PackageModule.{Definition => PackageDefinition, Specification => PackageSpecification}
import org.finos.morphir.ir.Type.{Definition as TypeDefinition, Specification as TypeSpecification, *}
import org.finos.morphir.ir.Value.{Definition => ValueDefinition, Specification => ValueSpecification}
import org.finos.morphir.ir.Value.{Value, _}
import org.finos.morphir.ir.module.{Definition => ModuleDefinition, Specification => ModuleSpecification}
import org.finos.morphir.ir.printing.PrintIR
import org.finos.morphir.ir.printing.DetailLevel
import org.finos.morphir.ir.printing.FieldNames
import org.finos.morphir.naming._

object EncodingExample {
  import MorphirJsonDecodingSupport._
  import MorphirJsonEncodingSupport._

  /*
    if True then
        [ 1.5 ]
    else
        [ 1 ]
   */
  def encode(): Unit = {
    object Refs {
      val `morphir.SDK`             = PackageName.fromString("morphir.SDK")
      val `basics`                  = ModuleName.fromString("basics")
      val `list`                    = ModuleName.fromString("list")
      val `morphir.SDK.basics.int`  = FQName(`morphir.SDK`, `basics`, Name("int"))
      val `morphir.SDK.basics.bool` = FQName(`morphir.SDK`, `basics`, Name("bool"))
      val `morphir.SDK.list.list`   = FQName(`morphir.SDK`, `list`, Name("list"))
    }

    val `sdk.Int`       = org.finos.morphir.ir.Type.reference(Refs.`morphir.SDK.basics.int`)
    val `sdk.Boolean`   = org.finos.morphir.ir.Type.reference(Refs.`morphir.SDK.basics.bool`)
    val `List[sdk.Int]` = org.finos.morphir.ir.Type.reference(Refs.`morphir.SDK.list.list`, `sdk.Int`)

    val trueCase =
      Value.List(
        `List[sdk.Int]`,
        Value.Literal(`sdk.Int`, Literal.floatLiteral(1.5))
      )

    val falseCase =
      Value.List(
        `List[sdk.Int]`,
        Value.Literal(`sdk.Int`, Literal.floatLiteral(1))
      )

    val ifThenElse =
      Value.IfThenElse.Typed(
        `sdk.Int`,
        Value.Literal(`sdk.Boolean`, Literal.boolLiteral(true)),
        trueCase,
        falseCase
      )

    println(ifThenElse.toJsonPretty)
  }

  // TODO Move to core
  def decode(): Unit = {
    val decoded =
      MorphirJsonDecodingSupport
        .valueDecoder[Unit, UType]
        .decodeJson(giganticString) match {
        case Right(value) => value
        case Left(error)  => throw new RuntimeException(error)
      }
    println(PrintIR(decoded, DetailLevel.BirdsEye.copy(showFieldNames = false)))
    ()
  }

  def main(args: Array[String]): Unit =
    decode()

  // IfThenElse(Type.Reference[Any]([], ""))

  /*
module My.Test exposing (..)
bar : List a
bar =
    if True then
        [ 1.5 ]
    else
        [ 1 ]
   */

  val giganticString =
    """
[
  "apply",
  ["Reference", [], [[["morphir"], ["s", "d", "k"]], [["basics"]], ["int"]], []],
  [
    "reference",
    [
      "Function",
      [],
      ["Reference", [], [[["morphir"], ["s", "d", "k"]], [["basics"]], ["int"]], []],
      ["Reference", [], [[["morphir"], ["s", "d", "k"]], [["basics"]], ["int"]], []]
    ],
    [[["my"]], [["test"]], ["fun"]]
  ],
  [
    "literal",
    ["Reference", [], [[["morphir"], ["s", "d", "k"]], [["basics"]], ["int"]], []],
    ["WholeNumberLiteral", 2]
  ]
]
    """
}
