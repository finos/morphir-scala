package org.finos.morphir.formats.internal.json

object visitors:
  abstract class Visitor[T]:
    def visitStr(value: String): T
    def visitNum(value: Int): T
    def visitObj(): ObjVisitor[T]

  abstract class ObjVisitor[T]:
    def visitKey(key: String): Unit

    def visitValue(): Visitor[T]
    def visitValue(value: T): Unit
    def done(): T
