package org.finos.morphir.ir.internal

import zio.Chunk
import zio.prelude.fx.ZPure

trait TransformValue[T, TA, VA] extends Transform[T] {
  import Transform._

  protected[this] def transform[R](c: R): Stateful[T, R]         = const(c)
  protected[this] def transformAttribute(c: VA): Stateful[T, VA] = const(c)

  def of(valueIn: Value[TA, VA]): Stateful[T, Value[TA, VA]] =
    valueIn match {
      case v: Value.Apply[_, _]         => of(v)
      case v: Value.Constructor[_]      => of(v)
      case v: Value.Destructure[_, _]   => of(v)
      case v: Value.Field[_, _]         => of(v)
      case v: Value.FieldFunction[_]    => of(v)
      case v: Value.IfThenElse[_, _]    => of(v)
      case v: Value.Lambda[_, _]        => of(v)
      case v: Value.LetDefinition[_, _] => of(v)
      case v: Value.LetRecursion[_, _]  => of(v)
      case v: Value.List[_, _]          => of(v)
      case v: Value.Literal[_]          => of(v)
      case v: Value.PatternMatch[_, _]  => of(v)
      case v: Value.Record[_, _]        => of(v)
      case v: Value.Reference[_]        => of(v)
      case v: Value.Tuple[_, _]         => of(v)
      case v: Value.Unit[_]             => of(v)
      case v: Value.UpdateRecord[_, _]  => of(v)
      case v: Value.Variable[_]         => of(v)
    }

  // ====== Leaf Cases ======
  def of(value: Value.FieldFunction[VA]): Stateful[T, Value.FieldFunction[VA]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
    } yield (v.copy(attributes = attr))

  def of(value: Value.Literal[VA]): Stateful[T, Value.Literal[VA]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
    } yield (v.copy(attributes = attr))

  def of(value: Value.Reference[VA]): Stateful[T, Value.Reference[VA]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
    } yield (v.copy(attributes = attr))

  def of(value: Value.Unit[VA]): Stateful[T, Value.Unit[VA]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
    } yield (v.copy(attributes = attr))

  def of(value: Value.Variable[VA]): Stateful[T, Value.Variable[VA]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
    } yield (v.copy(attributes = attr))

  // ====== Recursive Cases ======
  def of(value: Value.Apply[TA, VA]): Stateful[T, Value.Apply[TA, VA]] =
    for {
      v        <- transform(value)
      attr     <- transformAttribute(v.attributes)
      function <- of(v.function)
      argument <- of(v.argument)
    } yield Value.Apply(attr, function, argument)

  def of(value: Value.Destructure[TA, VA]): Stateful[T, Value.Destructure[TA, VA]] =
    for {
      v                  <- transform(value)
      attr               <- transformAttribute(v.attributes)
      valueToDestructure <- of(v.valueToDestruct)
      inValue            <- of(v.inValue)
    } yield Value.Destructure(attr, v.pattern, valueToDestructure, inValue)

  def of(value: Value.Field[TA, VA]): Stateful[T, Value.Field[TA, VA]] =
    for {
      v            <- transform(value)
      attr         <- transformAttribute(v.attributes)
      subjectValue <- of(v.subjectValue)
    } yield Value.Field(attr, subjectValue, v.fieldName)

  def of(value: Value.IfThenElse[TA, VA]): Stateful[T, Value.IfThenElse[TA, VA]] =
    for {
      v          <- transform(value)
      attr       <- transformAttribute(v.attributes)
      condition  <- of(v.condition)
      thenBranch <- of(v.thenBranch)
      elseBranch <- of(v.elseBranch)
    } yield Value.IfThenElse(attr, condition, thenBranch, elseBranch)

  def of(value: Value.Lambda[TA, VA]): Stateful[T, Value.Lambda[TA, VA]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
      body <- of(v.body)
    } yield Value.Lambda(attr, v.argumentPattern, body)

  def of(value: Value.LetDefinition[TA, VA]): Stateful[T, Value.LetDefinition[TA, VA]] =
    for {
      v               <- transform(value)
      attr            <- transformAttribute(v.attributes)
      valueDefinition <- of(v.valueDefinition.body).map(vd => v.valueDefinition.copy(body = vd))
      inValue         <- of(v.inValue)
    } yield Value.LetDefinition(attr, v.valueName, valueDefinition, inValue)

  def of(value: Value.LetRecursion[TA, VA]): Stateful[T, Value.LetRecursion[TA, VA]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
      valueDefinitions <- ofMapValues(v.valueDefinitions) { vdef =>
        of(vdef.body).map(body => vdef.copy(body = body))
      }
      inValue <- of(v.inValue)
    } yield Value.LetRecursion(attr, valueDefinitions, inValue)

  def of(value: Value.List[TA, VA]): Stateful[T, Value.List[TA, VA]] =
    for {
      v        <- transform(value)
      attr     <- transformAttribute(v.attributes)
      elements <- ofChunk(v.elements)(of(_))
    } yield Value.List(attr, elements)

  def of(value: Value.PatternMatch[TA, VA]): Stateful[T, Value.PatternMatch[TA, VA]] =
    for {
      v           <- transform(value)
      attr        <- transformAttribute(v.attributes)
      branchOutOn <- of(v.branchOutOn) // the 'subject' of the pattern-match i.e. the `x` in `match x ...`
      cases <- ofChunk(v.cases) { (casePattern, caseValue) =>
        of(caseValue).map(caseValue => (casePattern, caseValue))
      }
    } yield Value.PatternMatch(attr, branchOutOn, cases)

  def of(value: Value.Record[TA, VA]): Stateful[T, Value.Record[TA, VA]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
      fields <- ofChunk(v.fields) { (fieldName, fieldValue) =>
        of(fieldValue).map(fieldValue => (fieldName, fieldValue))
      }
    } yield Value.Record(attr, fields)

  def of(value: Value.Tuple[TA, VA]): Stateful[T, Value.Tuple[TA, VA]] =
    for {
      v        <- transform(value)
      attr     <- transformAttribute(v.attributes)
      elements <- ofChunk(v.elements)(of(_))
    } yield Value.Tuple(attr, elements)

  def of(value: Value.UpdateRecord[TA, VA]): Stateful[T, Value.UpdateRecord[TA, VA]] =
    for {
      v             <- transform(value)
      attr          <- transformAttribute(v.attributes)
      valueToUpdate <- of(v.valueToUpdate)
      updates       <- ofMapValues(v.fieldsToUpdate)(of(_))
    } yield Value.UpdateRecord(attr, valueToUpdate, updates)

  def of(value: Value.Constructor[VA]): Stateful[T, Value.Constructor[VA]] =
    for {
      v    <- transform(value)
      attr <- transformAttribute(v.attributes)
    } yield (v.copy(attributes = attr))
}
