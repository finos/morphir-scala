package org.finos.morphir.prelude

import org.finos.morphir.prelude.Chunk
import org.finos.morphir.prelude.NonEmptyChunk
import org.finos.morphir.testing.MorphirBaseSpec

import zio.test.Assertion._
//import zio.test.TestAspect.exceptScala211
import zio.test._

object ChunkSpec extends MorphirBaseSpec {

  case class Value(i: Int) extends AnyVal

  // import ZIOTag._

  val intGen: Gen[Any, Int] = Gen.int(-10, 10)

  def toBoolFn[R, A]: Gen[R, A => Boolean] =
    Gen.function(Gen.boolean)

  // def tinyChunks[R, A](a: Gen[R, A]): Gen[R, Chunk[A]] =
  //   Gen.chunkOfBounded(0, 3)(a)

  // def smallChunks[R, A](a: Gen[R, A]): Gen[R, Chunk[A]] =
  //   Gen.small(Gen.chunkOfN(_)(a))

  // def mediumChunks[R, A](a: Gen[R, A]): Gen[R, Chunk[A]] =
  //   Gen.medium(Gen.chunkOfN(_)(a))

  // def largeChunks[R, A](a: Gen[R, A]): Gen[R, Chunk[A]] =
  //   Gen.large(Gen.chunkOfN(_)(a))

  // def chunkWithIndex[R, A](a: Gen[R, A]): Gen[R, (Chunk[A], Int)] =
  //   for {
  //     chunk <- Gen.chunkOfBounded(1, 100)(a)
  //     idx   <- Gen.int(0, chunk.length - 1)
  //   } yield (chunk, idx)

  def spec = suite("ChunkSpec")(
    suite("size/length")(
      test("concatenated size must match length") {
        val chunk = Chunk.empty ++ Chunk.fromArray(Array(1, 2)) ++ Chunk(3, 4, 5) ++ Chunk.single(6)
        assert(chunk.size)(equalTo(chunk.length))
      },
      test("empty size must match length") {
        val chunk = Chunk.empty
        assert(chunk.size)(equalTo(chunk.length))
      },
      test("fromArray size must match length") {
        val chunk = Chunk.fromArray(Array(1, 2, 3))
        assert(chunk.size)(equalTo(chunk.length))
      },
      test("fromIterable size must match length") {
        val chunk = Chunk.fromIterable(List("1", "2", "3"))
        assert(chunk.size)(equalTo(chunk.length))
      },
      test("single size must match length") {
        val chunk = Chunk.single(true)
        assert(chunk.size)(equalTo(chunk.length))
      }
    ),
    suite("append")(
      // test("apply") {
      //   val chunksWithIndex: Gen[Any, (Chunk[Int], Chunk[Int], Int)] =
      //     for {
      //       p  <- Gen.boolean
      //       as <- Gen.chunkOf(Gen.int)
      //       bs <- Gen.chunkOf1(Gen.int)
      //       n  <- Gen.int(0, as.length + bs.length - 1)
      //     } yield if (p) (as, bs, n) else (bs, as, n)
      //   check(chunksWithIndex) { case (as, bs, n) =>
      //     val actual   = bs.foldLeft(as)(_ :+ _).apply(n)
      //     val expected = (as ++ bs).apply(n)
      //     assert(actual)(equalTo(expected))
      //   }
      // },
      // test("buffer full") {
      //   check(Gen.chunkOf(Gen.int), Gen.chunkOf(Gen.int)) { (as, bs) =>
      //     def addAll[A](l: Chunk[A], r: Chunk[A]): Chunk[A] = r.foldLeft(l)(_ :+ _)
      //     val actual                                        = List.fill(100)(bs).foldLeft(as)(addAll)
      //     val expected                                      = List.fill(100)(bs).foldLeft(as)(_ ++ _)
      //     assert(actual)(equalTo(expected))
      //   }
      // },
      // test("buffer used") {
      //   check(Gen.chunkOf(Gen.int), Gen.chunkOf(Gen.int)) { (as, bs) =>
      //     val effect   = ZIO.succeed(bs.foldLeft(as)(_ :+ _))
      //     val actual   = ZIO.collectAllPar(ZIO.replicate(100)(effect))
      //     val expected = as ++ bs
      //     assertZIO(actual)(forall(equalTo(expected)))
      //   }
      // },
      // test("equals") {
      //   check(Gen.chunkOf(Gen.int), Gen.chunkOf(Gen.int)) { (as, bs) =>
      //     val actual   = bs.foldLeft(as)(_ :+ _)
      //     val expected = as ++ bs
      //     assert(actual)(equalTo(expected))
      //   }
      // },
      // test("length") {
      //   check(Gen.chunkOf(Gen.int), smallChunks(Gen.int)) { (as, bs) =>
      //     val actual   = bs.foldLeft(as)(_ :+ _).length
      //     val expected = (as ++ bs).length
      //     assert(actual)(equalTo(expected))
      //   }
      // },
      test("returns most specific type") {
        val seq = (Chunk("foo"): Seq[String]) :+ "post1"
        assert(seq)(isSubtype[Chunk[String]](equalTo(Chunk("foo", "post1"))))
      },
      test("fails if the chunk does not contain the specified index") {
        val chunk    = Chunk(1, 2, 3)
        val appended = chunk :+ 4
        val _        = chunk :+ 5
        assert(appended(4))(throwsA[IndexOutOfBoundsException])
      }
    ),
    // suite("prepend")(
    // test("apply") {
    //   val chunksWithIndex: Gen[Any, (Chunk[Int], Chunk[Int], Int)] =
    //     for {
    //       p  <- Gen.boolean
    //       as <- Gen.chunkOf(Gen.int)
    //       bs <- Gen.chunkOf1(Gen.int)
    //       n  <- Gen.int(0, as.length + bs.length - 1)
    //     } yield if (p) (as, bs, n) else (bs, as, n)
    //   check(chunksWithIndex) { case (as, bs, n) =>
    //     val actual   = as.foldRight(bs)(_ +: _).apply(n)
    //     val expected = (as ++ bs).apply(n)
    //     assert(actual)(equalTo(expected))
    //   }
    // },
    // test("buffer full") {
    //   check(Gen.chunkOf(Gen.int), Gen.chunkOf(Gen.int)) { (as, bs) =>
    //     def addAll[A](l: Chunk[A], r: Chunk[A]): Chunk[A] = l.foldRight(r)(_ +: _)
    //     val actual                                        = List.fill(100)(as).foldRight(bs)(addAll)
    //     val expected                                      = List.fill(100)(as).foldRight(bs)(_ ++ _)
    //     assert(actual)(equalTo(expected))
    //   }
    // },
    // test("buffer used") {
    //   check(Gen.chunkOf(Gen.int), Gen.chunkOf(Gen.int)) { (as, bs) =>
    //     val effect   = ZIO.succeed(as.foldRight(bs)(_ +: _))
    //     val actual   = ZIO.collectAllPar(ZIO.replicate(100)(effect))
    //     val expected = as ++ bs
    //     assertZIO(actual)(forall(equalTo(expected)))
    //   }
    // },
    // test("equals") {
    //   check(Gen.chunkOf(Gen.int), Gen.chunkOf(Gen.int)) { (as, bs) =>
    //     val actual   = as.foldRight(bs)(_ +: _)
    //     val expected = as ++ bs
    //     assert(actual)(equalTo(expected))
    //   }
    // },
    // test("length") {
    //   check(Gen.chunkOf(Gen.int), smallChunks(Gen.int)) { (as, bs) =>
    //     val actual   = as.foldRight(bs)(_ +: _).length
    //     val expected = (as ++ bs).length
    //     assert(actual)(equalTo(expected))
    //   }
    // },
    // test("returns most specific type") {
    //   val seq = "pre1" +: (Chunk("foo"): Seq[String])
    //   assert(seq)(isSubtype[Chunk[String]](equalTo(Chunk("pre1", "foo"))))
    // },
    // test("fails if the chunk does not contain the specified index") {
    //   val chunk     = Chunk(1, 2, 3)
    //   val prepended = 0 +: chunk
    //   val _         = -1 +: chunk
    //   assert(prepended(-1))(throwsA[IndexOutOfBoundsException])
    // }
    // ),
    // test("apply") {
    //   check(chunkWithIndex(Gen.unit)) { case (chunk, i) =>
    //     assert(chunk.apply(i))(equalTo(chunk.toList.apply(i)))
    //   }
    // },
    // suite("specialized accessors")(
    //   test("boolean") {
    //     check(chunkWithIndex(Gen.boolean)) { case (chunk, i) =>
    //       assert(chunk.boolean(i))(equalTo(chunk.toList.apply(i)))
    //     }
    //   },
    //   test("byte") {
    //     check(chunkWithIndex(Gen.byte(0, 127))) { case (chunk, i) =>
    //       assert(chunk.byte(i))(equalTo(chunk.toList.apply(i)))
    //     }
    //   },
    //   test("char") {
    //     check(chunkWithIndex(Gen.char(33, 123))) { case (chunk, i) =>
    //       assert(chunk.char(i))(equalTo(chunk.toList.apply(i)))
    //     }
    //   },
    //   test("short") {
    //     check(chunkWithIndex(Gen.short(5, 100))) { case (chunk, i) =>
    //       assert(chunk.short(i))(equalTo(chunk.toList.apply(i)))
    //     }
    //   },
    //   test("int") {
    //     check(chunkWithIndex(Gen.int(1, 142))) { case (chunk, i) =>
    //       assert(chunk.int(i))(equalTo(chunk.toList.apply(i)))
    //     }
    //   },
    //   test("long") {
    //     check(chunkWithIndex(Gen.long(1, 142))) { case (chunk, i) =>
    //       assert(chunk.long(i))(equalTo(chunk.toList.apply(i)))
    //     }
    //   },
    //   test("float") {
    //     check(chunkWithIndex(Gen.double(0.0, 100.0).map(_.toFloat))) { case (chunk, i) =>
    //       assert(chunk.float(i))(equalTo(chunk.toList.apply(i)))
    //     }
    //   },
    //   test("double") {
    //     check(chunkWithIndex(Gen.double(1.0, 200.0))) { case (chunk, i) =>
    //       assert(chunk.double(i))(equalTo(chunk.toList.apply(i)))
    //     }
    //   }
    // ),
    // test("corresponds") {
    //   val genChunk = smallChunks(intGen)
    //   val genFunction =
    //     Gen.function[Any, (Int, Int), Boolean](Gen.boolean).map(Function.untupled(_))
    //   check(genChunk, genChunk, genFunction) { (as, bs, f) =>
    //     val actual   = as.corresponds(bs)(f)
    //     val expected = as.toList.corresponds(bs.toList)(f)
    //     assert(actual)(equalTo(expected))
    //   }
    // },
    test("fill") {
      val smallInt = Gen.int(-10, 10)
      check(smallInt, smallInt) { (n, elem) =>
        val actual   = Chunk.fill(n)(elem)
        val expected = Chunk.fromArray(Array.fill(n)(elem))
        assert(actual)(equalTo(expected))
      }
    },
    test("splitWhere") {
      assert(Chunk(1, 2, 3, 4).splitWhere(_ == 2))(equalTo((Chunk(1), Chunk(2, 3, 4))))
    },
    // test("length") {
    //   check(largeChunks(intGen))(chunk => assert(chunk.length)(equalTo(chunk.toList.length)))
    // },
    // test("equality") {
    //   check(mediumChunks(intGen), mediumChunks(intGen)) { (c1, c2) =>
    //     assert(c1.equals(c2))(equalTo(c1.toList.equals(c2.toList)))
    //   }
    // },
    test("inequality") {
      assert(Chunk(1, 2, 3, 4, 5))(Assertion.not(equalTo(Chunk(1, 2, 3, 4, 5, 6))))
    },
    // test("materialize") {
    //   check(mediumChunks(intGen))(c => assert(c.materialize.toList)(equalTo(c.toList)))
    // },
    // test("foldLeft") {
    //   check(Gen.alphaNumericString, Gen.function2(Gen.alphaNumericString), smallChunks(Gen.alphaNumericString)) {
    //     (s0, f, c) => assert(c.fold(s0)(f))(equalTo(c.toArray.foldLeft(s0)(f)))
    //   }
    // },
    test("foldRight") {
      val chunk    = Chunk("a") ++ Chunk("b") ++ Chunk("c")
      val actual   = chunk.foldRight("d")(_ + _)
      val expected = "abcd"
      assert(actual)(equalTo(expected))
    },
    test("mapAccum") {
      assert(Chunk(1, 1, 1).mapAccum(0)((s, el) => (s + el, s + el)))(equalTo((3, Chunk(1, 2, 3))))
    },
    // test("map") {
    //   val fn = Gen.function[Any, Int, Int](intGen)
    //   check(smallChunks(intGen), fn)((c, f) => assert(c.map(f).toList)(equalTo(c.toList.map(f))))
    // },
    // test("flatMap") {
    //   val fn = Gen.function[Any, Int, Chunk[Int]](smallChunks(intGen))
    //   check(smallChunks(intGen), fn) { (c, f) =>
    //     assert(c.flatMap(f).toList)(equalTo(c.toList.flatMap(f.andThen(_.toList))))
    //   }
    // },
    test("flatten") {
      val chunk = Chunk(Some(1), Some(2), None, None)
      assertTrue(chunk.flatten == Chunk(1, 2))
    },
    // test("headOption") {
    //   check(mediumChunks(intGen))(c => assert(c.headOption)(equalTo(c.toList.headOption)))
    // },
    // test("lastOption") {
    //   check(mediumChunks(intGen))(c => assert(c.lastOption)(equalTo(c.toList.lastOption)))
    // },
    // test("indexWhere") {
    //   val fn = Gen.function[Any, Int, Boolean](Gen.boolean)
    //   check(smallChunks(intGen), smallChunks(intGen), fn, intGen) { (left, right, p, from) =>
    //     val actual   = (left ++ right).indexWhere(p, from)
    //     val expected = (left.toVector ++ right.toVector).indexWhere(p, from)
    //     assert(actual)(equalTo(expected))
    //   }
    // } @@ exceptScala211,
    // test("exists") {
    //   val fn = Gen.function[Any, Int, Boolean](Gen.boolean)
    //   check(mediumChunks(intGen), fn)((chunk, p) => assert(chunk.exists(p))(equalTo(chunk.toList.exists(p))))
    // },
    // test("forall") {
    //   val fn = Gen.function[Any, Int, Boolean](Gen.boolean)
    //   check(mediumChunks(intGen), fn)((chunk, p) => assert(chunk.forall(p))(equalTo(chunk.toList.forall(p))))
    // },
    // test("find") {
    //   val fn = Gen.function[Any, Int, Boolean](Gen.boolean)
    //   check(mediumChunks(intGen), fn)((chunk, p) => assert(chunk.find(p))(equalTo(chunk.toList.find(p))))
    // },
    // test("filter") {
    //   val fn = Gen.function[Any, Int, Boolean](Gen.boolean)
    //   check(mediumChunks(intGen), fn)((chunk, p) => assert(chunk.filter(p).toList)(equalTo(chunk.toList.filter(p))))
    // },
    // test("drop chunk") {
    //   check(largeChunks(intGen), intGen)((chunk, n) => assert(chunk.drop(n).toList)(equalTo(chunk.toList.drop(n))))
    // },
    // test("dropRight chunk") {
    //   check(largeChunks(intGen), intGen)((chunk, n) =>
    //     assert(chunk.dropRight(n).toList)(equalTo(chunk.toList.dropRight(n)))
    //   )
    // },
    // test("take chunk") {
    //   check(chunkWithIndex(Gen.unit)) { case (c, n) =>
    //     assert(c.take(n).toList)(equalTo(c.toList.take(n)))
    //   }
    // },
    // test("takeRight chunk") {
    //   check(chunkWithIndex(Gen.unit)) { case (c, n) =>
    //     assert(c.takeRight(n).toList)(equalTo(c.toList.takeRight(n)))
    //   }
    // },
    // test("dropWhile chunk") {
    //   check(mediumChunks(intGen), toBoolFn[Any, Int]) { (c, p) =>
    //     assert(c.dropWhile(p).toList)(equalTo(c.toList.dropWhile(p)))
    //   }
    // },
    // test("takeWhile chunk") {
    //   check(mediumChunks(intGen), toBoolFn[Any, Int]) { (c, p) =>
    //     assert(c.takeWhile(p).toList)(equalTo(c.toList.takeWhile(p)))
    //   }
    // },
    // test("toArray") {
    //   check(mediumChunks(Gen.alphaNumericString))(c => assert(c.toArray.toList)(equalTo(c.toList)))
    // },
    test("non-homogeneous element type") {
      trait Animal
      trait Cat extends Animal
      trait Dog extends Animal

      val vector   = Vector(new Cat {}, new Dog {}, new Animal {})
      val actual   = Chunk.fromIterable(vector).map(identity)
      val expected = Chunk.fromArray(vector.toArray)

      assert(actual)(equalTo(expected))
    },
    test("toArray for an empty Chunk of type String") {
      assert(Chunk.empty[String].toArray)(equalTo(Array.empty[String]))
    },
    test("to Array for an empty Chunk using filter") {
      assert(Chunk(1).filter(_ == 2).map(_.toString).toArray[String])(equalTo(Array.empty[String]))
    },
    // test("toArray with elements of type String") {
    //   check(mediumChunks(Gen.alphaNumericString))(c => assert(c.toArray.toList)(equalTo(c.toList)))
    // },
    test("toArray for a Chunk of any type") {
      val v: Vector[Any] = Vector("String", 1, Value(2))
      assert(Chunk.fromIterable(v).toArray.toVector)(equalTo(v))
    },
    suite("collect")(
      test("collect empty Chunk") {
        assert(Chunk.empty[Nothing].collect { case _ => 1 })(isEmpty)
      }
      // test("collect chunk") {
      //   val pfGen = Gen.partialFunction[Any, Int, Int](intGen)
      //   check(mediumChunks(intGen), pfGen)((c, pf) => assert(c.collect(pf).toList)(equalTo(c.toList.collect(pf))))
      // }
    ),
    suite("collectWhile")(
      test("collectWhile empty Chunk") {
        assert(Chunk.empty[Nothing].collectWhile { case _ => 1 })(isEmpty)
      }
      // test("collectWhile chunk") {
      //   val pfGen = Gen.partialFunction[Any, Int, Int](intGen)
      //   check(mediumChunks(intGen), pfGen) { (c, pf) =>
      //     assert(c.collectWhile(pf).toList)(equalTo(c.toList.takeWhile(pf.isDefinedAt).map(pf.apply)))
      //   }
      // }
    ),
    // test("foreach") {
    //   check(mediumChunks(intGen)) { c =>
    //     var sum = 0
    //     c.foreach(sum += _)

    //     assert(sum)(equalTo(c.toList.sum))
    //   }
    // },
    // test("concat chunk") {
    //   check(smallChunks(intGen), smallChunks(intGen)) { (c1, c2) =>
    //     assert((c1 ++ c2).toList)(equalTo(c1.toList ++ c2.toList))
    //   }
    // },
    test("chunk transitivity") {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      val c3 = Chunk(1, 2, 3)
      assert(c1 == c2 && c2 == c3 && c1 == c3)(Assertion.isTrue)
    },
    test("chunk symmetry") {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      assert(c1 == c2 && c2 == c1)(Assertion.isTrue)
    },
    test("chunk reflexivity") {
      val c1 = Chunk(1, 2, 3)
      assert(c1 == c1)(Assertion.isTrue)
    },
    test("chunk negation") {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      assert(c1 != c2 == !(c1 == c2))(Assertion.isTrue)
    },
    test("chunk substitutivity") {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      assert(c1 == c2 && c1.toString == c2.toString)(Assertion.isTrue)
    },
    test("chunk consistency") {
      val c1 = Chunk(1, 2, 3)
      val c2 = Chunk(1, 2, 3)
      assert(c1 == c2 && c1.hashCode == c2.hashCode)(Assertion.isTrue)
    },
    test("nullArrayBug") {
      val c = Chunk.fromArray(Array(1, 2, 3, 4, 5))

      // foreach should not throw
      c.foreach(_ => ())

      assert(c.filter(_ => false).map(_ * 2).length)(equalTo(0))
    },
    test("toArrayOnConcatOfSlice") {
      val onlyOdd: Int => Boolean = _ % 2 != 0
      val concat = Chunk(1, 1, 1).filter(onlyOdd) ++
        Chunk(2, 2, 2).filter(onlyOdd) ++
        Chunk(3, 3, 3).filter(onlyOdd)

      val array = concat.toArray

      assert(array)(equalTo(Array(1, 1, 1, 3, 3, 3)))
    },
    test("toArrayOnConcatOfEmptyAndInts") {
      assert(Chunk.empty ++ Chunk.fromArray(Array(1, 2, 3)))(equalTo(Chunk(1, 2, 3)))
    },
    test("filterConstFalseResultsInEmptyChunk") {
      assert(Chunk.fromArray(Array(1, 2, 3)).filter(_ => false))(equalTo(Chunk.empty))
    },
    test("zip") {
      check(Gen.chunkOf(Gen.int), Gen.chunkOf(Gen.int)) { (as, bs) =>
        val actual   = as.zip(bs).toList
        val expected = as.toList.zip(bs.toList)
        assert(actual)(equalTo(expected))
      }
    },
    test("zipAll") {
      val a = Chunk(1, 2, 3)
      val b = Chunk(1, 2)
      val c = Chunk((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3)))
      val d = Chunk((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), None))
      val e = Chunk((Some(1), Some(1)), (Some(2), Some(2)), (None, Some(3)))
      assert(a.zipAll(a))(equalTo(c)) &&
      assert(a.zipAll(b))(equalTo(d)) &&
      assert(b.zipAll(a))(equalTo(e))
    },
    test("zipAllWith") {
      assert(Chunk(1, 2, 3).zipAllWith(Chunk(3, 2, 1))(_ => 0, _ => 0)(_ + _))(equalTo(Chunk(4, 4, 4))) &&
      assert(Chunk(1, 2, 3).zipAllWith(Chunk(3, 2))(_ => 0, _ => 0)(_ + _))(equalTo(Chunk(4, 4, 0))) &&
      assert(Chunk(1, 2).zipAllWith(Chunk(3, 2, 1))(_ => 0, _ => 0)(_ + _))(equalTo(Chunk(4, 4, 0)))
    },
    test("zipWithIndex") {
      val (ch1, ch2) = Chunk("a", "b", "c", "d").splitAt(2)
      val ch         = ch1 ++ ch2
      assert(ch.zipWithIndex.toList)(equalTo(ch.toList.zipWithIndex))
    },
    // test("zipWithIndex on concatenated chunks") {
    //   check(smallChunks(intGen), smallChunks(intGen)) { (c1, c2) =>
    //     val items   = (c1 ++ c2).zipWithIndex.map(_._1)
    //     val indices = (c1 ++ c2).zipWithIndex.map(_._2)

    //     assert(items.toList)(equalTo(c1.toList ++ c2.toList)) &&
    //     assert(indices.toList)(equalTo((0 until (c1.size + c2.size)).toList))
    //   }
    // },
    // test("zipWithIndexFrom on concatenated chunks") {
    //   check(smallChunks(intGen), smallChunks(intGen), Gen.int(0, 10)) { (c1, c2, from) =>
    //     val items   = (c1 ++ c2).zipWithIndexFrom(from).map(_._1)
    //     val indices = (c1 ++ c2).zipWithIndexFrom(from).map(_._2)

    //     assert(items.toList)(equalTo(c1.toList ++ c2.toList)) &&
    //     assert(indices.toList)(equalTo((from until (c1.size + c2.size + from)).toList))
    //   }
    // },
    test("partitionMap") {
      val as       = Chunk(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      val (bs, cs) = as.partitionMap(n => if (n % 2 == 0) Left(n) else Right(n))
      assert(bs)(equalTo(Chunk(0, 2, 4, 6, 8))) &&
      assert(cs)(equalTo(Chunk(1, 3, 5, 7, 9)))
    },
    test("stack safety concat") {
      val n  = 100000
      val as = List.range(0, n).foldRight[Chunk[Int]](Chunk.empty)((a, as) => Chunk(a) ++ as)
      assert(as.toArray)(equalTo(Array.range(0, n)))
    },
    test("stack safety append") {
      val n  = 100000
      val as = List.range(0, n).foldRight[Chunk[Int]](Chunk.empty)((a, as) => as :+ a)
      assert(as.toArray)(equalTo(Array.range(0, n).reverse))
    },
    test("stack safety prepend") {
      val n  = 100000
      val as = List.range(0, n).foldRight[Chunk[Int]](Chunk.empty)((a, as) => a +: as)
      assert(as.toArray)(equalTo(Array.range(0, n)))
    },
    test("stack safety concat and append") {
      val n = 100000
      val as = List.range(0, n).foldRight[Chunk[Int]](Chunk.empty) { (a, as) =>
        if (a % 2 == 0) as :+ a else as ++ Chunk(a)
      }
      assert(as.toArray)(equalTo(Array.range(0, n).reverse))
    },
    test("stack safety concat and prepend") {
      val n = 100000
      val as = List.range(0, n).foldRight[Chunk[Int]](Chunk.empty) { (a, as) =>
        if (a % 2 == 0) a +: as else Chunk(a) ++ as
      }
      assert(as.toArray)(equalTo(Array.range(0, n)))
    },
    test("toArray does not throw ClassCastException") {
      val chunk = Chunk("a")
      val array = chunk.toArray
      assert(array)(anything)
    },
    test("Tags.fromValue is safe on Scala.is") {
      val _ = Chunk(1, 128)
      assertCompletes
    },
    test("chunks can be constructed from heterogeneous collections") {
      check(Gen.listOf(Gen.oneOf(Gen.int, Gen.string, Gen.none))) { as =>
        assert(Chunk.fromIterable(as).toList)(equalTo(as))
      }
    },
    test("unfold") {
      assert(
        Chunk.unfold(0)(n => if (n < 10) Some((n, n + 1)) else None)
      )(equalTo(Chunk.fromIterable(0 to 9)))
    },
    test("split") {
      val smallInts = Gen.small(n => Gen.const(n), 1)
      val chunks    = Gen.chunkOf(Gen.int)
      check(smallInts, chunks) { (n, chunk) =>
        val groups = chunk.split(n)
        assert(groups.flatten)(equalTo(chunk)) &&
        assert(groups.size)(equalTo(n min chunk.length))
      }
    },
    // test("fromIterator") {
    //   check(Gen.chunkOf(Gen.int)) { as =>
    //     assert(Chunk.fromIterator(as.iterator))(equalTo(as))
    //   }
    // },
    test("fromIterable should works with Iterables traversing only once") {
      val traversableOnceIterable = new Iterable[Int] {
        val it = new Iterator[Int] {
          var c: Int                    = 3
          override def hasNext: Boolean = c > 0
          override def next(): Int      = { c = c - 1; c }
        }
        override def iterator: Iterator[Int] = it
      }
      assert(Chunk.fromIterable(traversableOnceIterable))(equalTo(Chunk(2, 1, 0)))
    },
    suite("unapplySeq")(
      test("matches a nonempty chunk") {
        val chunk = Chunk(1, 2, 3)
        val actual = chunk match {
          case Chunk(x, y, z) => Some((x, y, z))
          case _              => None
        }
        val expected = Some((1, 2, 3))
        assert(actual)(equalTo(expected))
      },
      test("matches an empty chunk") {
        val chunk = Chunk.empty
        val actual = chunk match {
          case Chunk() => Some(())
          case _       => None
        }
        val expected = Some(())
        assert(actual)(equalTo(expected))
      }
    ),
    suite("updated")(
      test("updates the chunk at the specified index") {
        check(Gen.chunkOfN(100)(Gen.int), Gen.listOf(Gen.int(0, 99)), Gen.listOf(Gen.int)) { (chunk, indices, values) =>
          val actual =
            indices.zip(values).foldLeft(chunk) { case (chunk, (index, value)) => chunk.updated(index, value) }
          val expected =
            indices.zip(values).foldLeft(chunk.toList) { case (chunk, (index, value)) => chunk.updated(index, value) }
          assert(actual.toList)(equalTo(expected))
        }
      } @@ TestAspect.flaky,
      test("fails if the chunk does not contain the specified index") {
        val chunk = Chunk(1, 2, 3)
        assert(chunk.updated(3, 4))(throwsA[IndexOutOfBoundsException])
      },
      test("apply") {
        val chunk = Chunk.fill(256)(1).foldLeft(Chunk(0)) { case (as, a) =>
          as.updated(0, as(0) + a)
        }
        assertTrue(chunk(0) == 256)
      },
      test("buffer size") {
        val chunk = Chunk.fill(257)(1).zipWithIndex.foldLeft(Chunk.fill(256)(0)) { case (as, (a, i)) =>
          as.updated(i % 256, as(i % 256) + a)
        }
        assertTrue(chunk.sum == 257)
      }
    )
  )
}
