package org.finos.morphir.datamodel

import zio.prelude.fx.ZPure

trait ConceptStatefulTransformer[T] {

  type Stateful[A] = ZPure[Nothing, T, T, Any, Nothing, A]
  object Stateful {
    def const[A](a: A): Stateful[A] = ZPure.succeed(a)
    def succeedWithState[A, B](a: A)(stateChange: T => T): Stateful[A] =
      ZPure.update(stateChange) *> ZPure.succeed(a)
  }

  protected[this] def transform[R <: Concept](c: R): Stateful[R] = Stateful.const(c)

  def of(c: Concept): Stateful[Concept] =
    c match {
      case c: Concept.Basic[_] => of(c)
      case c: Concept.Any.type => of(c)
      case c: Concept.Record   => of(c)
      case c: Concept.Struct   => of(c)
      case c: Concept.Alias    => of(c)
      case c: Concept.List     => of(c)
      case c: Concept.Map      => of(c)
      case c: Concept.Tuple    => of(c)
      case c: Concept.Optional => of(c)
      case c: Concept.Result => of(c)
      case c: Concept.Enum     => of(c)
      case c: Concept.Union    => of(c)
    }

  def of(c: Concept.Basic[_]): Stateful[Concept.Basic[_]] =
    Stateful.const(c)

  def of(c: Concept.Any.type): Stateful[Concept.Any.type] =
    Stateful.const(c)

  def of(c: Concept.Record): Stateful[Concept.Record] =
    for {
      c      <- transform(c)
      fields <- ofFieldList(c.fields)(of)
    } yield Concept.Record(c.namespace, fields)

  def of(c: Concept.Struct): Stateful[Concept.Struct] =
    for {
      // This does not go into the output because a struct has no identity other than it's fields
      // It is still needed so that things like Concept.find will work. Also it will be used
      // in the future when metadata is introduced.
      c      <- transform(c)
      fields <- ofFieldList(c.fields)(of)
    } yield Concept.Struct(fields)

  def of(c: Concept.Alias): Stateful[Concept.Alias] =
    for {
      c    <- transform(c)
      body <- of(c.value)
    } yield Concept.Alias(c.name, body)

  def of(c: Concept.List): Stateful[Concept.List] =
    for {
      c    <- transform(c)
      elem <- of(c.elementType)
    } yield Concept.List(elem)

  def of(c: Concept.Map): Stateful[Concept.Map] =
    for {
      c <- transform(c)
      k <- of(c.keyType)
      v <- of(c.valueType)
    } yield Concept.Map(k, v)

  def of(c: Concept.Tuple): Stateful[Concept.Tuple] =
    for {
      c      <- transform(c)
      values <- ofList(c.values)(of)
    } yield Concept.Tuple(values)

  def of(c: Concept.Optional): Stateful[Concept.Optional] =
    for {
      c    <- transform(c)
      elem <- of(c.elementType)
    } yield Concept.Optional(elem)

  def of(c: Concept.Result): Stateful[Concept.Result] =
    for {
      c <- transform(c)
      err <- of(c.errType)
      ok <- of(c.okType)
    } yield Concept.Result(err, ok)

  def of(c: Concept.Enum): Stateful[Concept.Enum] =
    for {
      c <- transform(c)
      fields <- {
        val mappedCases =
          c.cases.map(enumCase =>
            (enumCase.label, ofFieldList(enumCase.fields)(of))
          )
        mappedCases.foldLeft(Stateful.const(List[Concept.Enum.Case]())) {
          case (pureList, (label, pureFields)) =>
            for {
              list   <- pureList
              fields <- pureFields
            } yield (list :+ Concept.Enum.Case(label, fields))
        }
      }
    } yield Concept.Enum(c.name, fields)

  def of(c: Concept.Union): Stateful[Concept.Union] =
    for {
      c      <- transform(c)
      values <- ofList(c.cases)(of)
    } yield Concept.Union(values)

  def ofList[A](list: List[A])(mapping: A => Stateful[A]) =
    list.foldLeft(Stateful.const(List[A]())) { (pureList, elem) =>
      for {
        list  <- pureList
        elem1 <- mapping(elem)
      } yield (list :+ elem1)
    }

  def ofFieldList[L, A](fields: List[(L, A)])(mapping: A => Stateful[A]) = {
    val concepts = fields.map(_._2)
    for {
      newConcepts <- ofList(concepts)(mapping(_))
    } yield {
      val newFields = fields.zip(newConcepts).map {
        case ((label, _), concept) => (label, concept)
      }
      newFields
    }
  }
}
