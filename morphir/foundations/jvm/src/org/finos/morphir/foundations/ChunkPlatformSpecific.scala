/*
 * Copyright 2023 Morphir Contributors
 * Copyright 2018-2023 John A. De Goes and the ZIO Contributors
 * All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 */
package org.finos.morphir.foundations

/**
 * Ported from ZIO: https://github.com/zio/zio/blob/series/2.x/core/js/src/main/scala/zio/ChunkPlatformSpecific.scala
 */

// import zio.stacktracer.TracingImplicits.disableAutoTrace

import scala.reflect.{ClassTag, classTag}

private[foundations] trait ChunkPlatformSpecific {

  private[foundations] object Tags {
    def fromValue[A](a: A): ClassTag[A] = {
      val c = a.getClass
      val unboxedClass =
        if (isBoolean(c)) BooleanClass.asInstanceOf[Class[A]]
        else if (isByte(c)) ByteClass.asInstanceOf[Class[A]]
        else if (isShort(c)) ShortClass.asInstanceOf[Class[A]]
        else if (isInt(c)) IntClass.asInstanceOf[Class[A]]
        else if (isLong(c)) LongClass.asInstanceOf[Class[A]]
        else if (isFloat(c)) FloatClass.asInstanceOf[Class[A]]
        else if (isDouble(c)) DoubleClass.asInstanceOf[Class[A]]
        else if (isChar(c)) CharClass.asInstanceOf[Class[A]]
        else null

      if (unboxedClass eq null) classTag[AnyRef].asInstanceOf[ClassTag[A]]
      else ClassTag(unboxedClass).asInstanceOf[ClassTag[A]]
    }

    private def isBoolean(c: Class[_]): Boolean =
      c == BooleanClass || c == BooleanClassBox
    private def isByte(c: Class[_]): Boolean =
      c == ByteClass || c == ByteClassBox
    private def isShort(c: Class[_]): Boolean =
      c == ShortClass || c == ShortClassBox
    private def isInt(c: Class[_]): Boolean =
      c == IntClass || c == IntClassBox
    private def isLong(c: Class[_]): Boolean =
      c == LongClass || c == LongClassBox
    private def isFloat(c: Class[_]): Boolean =
      c == FloatClass || c == FloatClassBox
    private def isDouble(c: Class[_]): Boolean =
      c == DoubleClass || c == DoubleClassBox
    private def isChar(c: Class[_]): Boolean =
      c == CharClass || c == CharClassBox

    private val BooleanClass    = classOf[Boolean]
    private val BooleanClassBox = classOf[java.lang.Boolean]
    private val ByteClass       = classOf[Byte]
    private val ByteClassBox    = classOf[java.lang.Byte]
    private val ShortClass      = classOf[Short]
    private val ShortClassBox   = classOf[java.lang.Short]
    private val IntClass        = classOf[Int]
    private val IntClassBox     = classOf[java.lang.Integer]
    private val LongClass       = classOf[Long]
    private val LongClassBox    = classOf[java.lang.Long]
    private val FloatClass      = classOf[Float]
    private val FloatClassBox   = classOf[java.lang.Float]
    private val DoubleClass     = classOf[Double]
    private val DoubleClassBox  = classOf[java.lang.Double]
    private val CharClass       = classOf[Char]
    private val CharClassBox    = classOf[java.lang.Character]
  }
}
