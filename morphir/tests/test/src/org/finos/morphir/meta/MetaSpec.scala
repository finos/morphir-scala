package org.finos.morphir.meta

import org.finos.morphir.testing.MorphirBaseSpec
import zio.Tag
import zio.test.*

import scala.annotation.Annotation
object MetaSpec extends MorphirBaseSpec {
  import MetaTestExamples._
  import Gaz._
  def spec = suite("MetaSpec")(
    test("Meta[T].name")(
      assertTrue(
        Meta[Gaz].name == "Gaz",
        Meta[Foo].name == "Foo",
        Meta[Bar].name == "Bar",
        Meta[|++|].name == "|++|",
        Meta[Car.type].name == "Car"
      )
    ),
    test("Meta[T].annotations")(
      assertTrue(
        Meta[Gaz].annotations == List(field("sealed trait Gaz")),
        Meta[Foo].annotations == List(field("case class Foo")),
        Meta[Bar].annotations == List(field("case class Bar")),
        Meta[|++|].annotations == List(field("case class |++|")),
        Meta[Car.type].annotations == List(field("case object Car"))
      )
    ),
    test("Test Meta[T].fieldNames")(
      assertTrue(
        Meta[Gaz].fieldNames.toList == List(),
        Meta[Foo].fieldNames.toList == List("s"),
        Meta[Bar].fieldNames.toList == List("txt", "<+>"),
        Meta[|++|].fieldNames.toList == List(),
        Meta[Car.type].fieldNames.toList == List()
      )
    ),
    test("Test Meta[T].fieldAnnotations")(
      assertTrue(
        Meta[Gaz].fieldAnnotations.toList == Nil,
        Meta[Foo].fieldAnnotations.toList == List(List(field("case class field s"))),
        Meta[Bar].fieldAnnotations.toList == List(
          List(field("case class field txt")),
          List(field("case class field <+>"))
        ),
        Meta[|++|].fieldAnnotations.toList == Nil,
        Meta[Car.type].fieldAnnotations.toList == Nil
      )
    )
  )
}

object MetaTestExamples {
  final case class field(name: String) extends Annotation

  @field("sealed trait Gaz")
  sealed trait Gaz
  object Gaz {
    @field("case class Foo")
    case class Foo(@field("case class field s") s: String) extends Gaz
    @field("case class Bar")
    case class Bar(@field("case class field txt") txt: String, @field("case class field <+>") `<+>`: Long) extends Gaz
    @field("case class |++|")
    case class |++|() extends Gaz
    @field("case object Car")
    case object Car extends Gaz
  }
}
