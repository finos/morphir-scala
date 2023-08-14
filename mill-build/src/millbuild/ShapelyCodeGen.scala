package millbuild

object ShapelyCodeGen {
  val product_arity = 64
  val sum_arity     = 64

  def data: String = {
    val caseclasses = (1 to product_arity).map { i =>
      val tparams = (1 to i).map(p => s"A$p").mkString(", ")
      val defns   = (1 to i).map(p => s"_$p: A$p").mkString(", ")
      val tuple   = (1 to i).map(p => s"_$p").mkString(", ")
      val untuple = (1 to i).map(p => s"t._$p").mkString(", ")
      // scala 2 is limited to tuples of arity 22. We could generate this for
      // scala 3 but we'd rather not have a different API.
      val withs =
        if (i > 22) s""
        else s"with Product${i}[$tparams] "
      val tupler =
        if (i > 22) ""
        else s"  def tuple: Tuple${i}[$tparams] = Tuple$i($tuple)\n"
      val value = (1 to i).map(p => s"case ${p - 1} => _$p")
      val body_object =
        if (i > 22) ""
        else
          s"object CaseClass$i { def untuple[A, $tparams](t: Tuple$i[$tparams]): CaseClass$i[A, $tparams] = CaseClass$i($untuple) }"
      s"""final case class CaseClass$i[A, $tparams]($defns) extends CaseClass[A] $withs{
         |  override def value(i: Int): Any = (i: @switch) match {
         |    ${value.mkString("\n    ")}
         |    case _ => throw new IllegalArgumentException
         |  }
         |$tupler
         |}
         |$body_object""".stripMargin
    }
    val sealedtraits = (1 to sum_arity).map { i =>
      val tparams  = (1 to i).map(p => s"A$p <: A").mkString(", ")
      val tparams_ = (1 to i).map(p => s"A$p").mkString(", ")
      val either   = (1 until i).foldRight(s"A$i" + ("]" * (i - 1)))((e, acc) => s"Either[A$e, $acc")
      def rights(pp: Int, s: String): String = if (pp <= 0) s else rights(pp - 1, s"Right($s)")
      def either_cons(p: Int, splice: String): String =
        if (p == i) rights(p - 1, splice) else rights(p - 1, s"Left($splice)")
      val toEithers   = (1 to i).map { p => s"""case SealedTrait._$p(v) => ${either_cons(p, "v")}""" }
      val fromEithers = (1 to i).map { p => s"""case ${either_cons(p, s"v")} => SealedTrait._$p(v)""" }

      // the either convertors are very inefficient and should only be used for small arity
      val body_object =
        if (i > 12) ""
        else s"""
                |object SealedTrait$i {
                |  def either[A, $tparams](st: SealedTrait$i[A, $tparams_]): $either = st match {
                |    ${toEithers.mkString("\n    ")}
                |  }
                |
                |  def uneither[A, $tparams](e: $either): SealedTrait$i[A, $tparams_] = e match {
                |    ${fromEithers.mkString("\n    ")}
                |  }
                |}""".stripMargin

      s"""sealed trait SealedTrait$i[A, $tparams] extends SealedTrait[A]$body_object"""
    }
    // this encoding idea thanks to Georgi Krastev
    val sealedtrait_cases = (1 to sum_arity).map { i =>
      val tparams  = (1 to sum_arity).map(p => s"A$p <: A").mkString(", ")
      val tparams_ = (1 to sum_arity).map(p => s"A$p")
      val parents = (i to sum_arity).map { p =>
        val ptparams = tparams_.take(p).mkString(", ")
        s"""SealedTrait$p[A, $ptparams]"""
      }.mkString(" with ")
      s"  final case class _$i[A, $tparams](value: A$i) extends $parents { override def index: Int = ${i - 1} }"
    }
    s"""package org.finos.morphir.meta
       |
       |import scala.annotation._
       |
       |sealed trait Shape[A]
       |sealed trait CaseClass[A] extends Shape[A] { def value(i: Int): Any }
       |sealed trait SealedTrait[A] extends Shape[A] { def value: A ; def index: Int }
       |
       |final case class CaseClass0[A]() extends CaseClass[A] { 
       |  @nowarn
       |  def tuple: Unit = () ; 
       |  override def value(i: Int): Any = throw new IllegalArgumentException 
       |}
       |case object CaseClass0 { def untuple[A](): CaseClass0[A] = apply[A]() }
       |
       |object CaseClass {
       |}
       |${caseclasses.mkString("\n\n")}
       |
       |object SealedTrait {
       |${sealedtrait_cases.mkString("\n\n")}
       |}
       |
       |${sealedtraits.mkString("\n\n")}
       |""".stripMargin
  }

  def compat: String = {
    val caseclasses = (1 to product_arity).map { i =>
      val tparams          = (1 to i).map(p => s"A$p").mkString(", ")
      def tcons(s: String) = if (i == 1) s"Tuple1[$s]" else s"($s)"

      s"""  implicit def caseclass$i[A <: Product, $tparams](implicit A: Mirror.ProductOf[A], ev1: A.MirroredElemTypes =:= ${tcons(
          tparams
        )}, S: Mirror.ProductOf[CaseClass$i[A, $tparams]]): Shapely[A, CaseClass$i[A, $tparams]] = new Shapely[A, CaseClass$i[A, $tparams]] {
         |    def to(a: A): CaseClass$i[A, $tparams] = S.fromProduct(a)
         |    def from(s: CaseClass$i[A, $tparams]): A = A.fromProduct(s)
         |  }""".stripMargin
    }
    val sealedtraits = (1 to sum_arity).map { i =>
      val tparams          = (1 to i).map(p => s"A$p <: A").mkString(", ")
      val tparams_         = (1 to i).map(p => s"A$p").mkString(", ")
      def tcons(i: String) = if (i == 1) s"Tuple1[$i]" else s"($i)"

      val to_matchers = (1 to i).map(p => s"  case ${p - 1} => SealedTrait._$p(a.asInstanceOf[A$p])")

      s"""  implicit def sealedtrait$i[A, $tparams](implicit A: Mirror.SumOf[A], ev: A.MirroredElemTypes =:= ${tcons(
          tparams_
        )}): Shapely[A, SealedTrait$i[A, $tparams_]] = new Shapely[A, SealedTrait$i[A, $tparams_]] {
         |    def to(a: A): SealedTrait$i[A, $tparams_] = A.ordinal(a) match {
         |    ${to_matchers.mkString("\n    ")}
         |    }
         |    def from(s: SealedTrait$i[A, $tparams_]): A = s.value
         |  }""".stripMargin
    }
    s"""package org.finos.morphir.meta
       |
       |import deriving.Mirror
       |
       |private[meta] trait ShapelyCompat { this: Shapely.type =>
       |
       |  implicit def caseclass0[A <: Product](implicit A: Mirror.ProductOf[A], ev: A.MirroredElemTypes =:= EmptyTuple): Shapely[A, CaseClass0[A]] = new Shapely[A, CaseClass0[A]] {
       |    def to(a: A): CaseClass0[A] = CaseClass0[A]()
       |    def from(s: CaseClass0[A]): A = A.fromProduct(s)
       |  }
       |
       |${caseclasses.mkString("\n\n")}
       |
       |${sealedtraits.mkString("\n\n")}
       |
       |}""".stripMargin
  }

  def derivable: String = {
    val caseclasses = (3 to product_arity).map { i =>
      val tparams = (1 to i).map(p => s"A$p").mkString(", ")
      val Fs      = (1 to i).map(p => s"F$p: Lazy[F[A$p]]").mkString(", ")

      if (i % 2 == 0) {
        val left_Fs     = (1 to i / 2).map(p => s"F$p").mkString(", ")
        val left        = s"""caseclass${i / 2}(X, A, $left_Fs)"""
        val left_cons_f = (1 to i / 2).map(p => s"a._$p").mkString(", ")
        val left_cons_g = (1 to i / 2).map(p => s"c._$p").mkString(", ")

        val right_Fs     = ((1 + i / 2) to i).map(p => s"F$p").mkString(", ")
        val right        = s"""caseclass${i / 2}(X, A, $right_Fs)"""
        val right_cons_f = (1 to i / 2).map(p => s"b._$p").mkString(", ")
        val right_cons_g = ((1 + i / 2) to i).map(p => s"c._$p").mkString(", ")

        s"""  implicit def caseclass$i[A, $tparams](implicit X: XFunctor[F], A: Align[F], $Fs): F[CaseClass$i[A, $tparams]] =
           |    X.xmap(A.align($left, $right))(
           |      { case (a, b) => CaseClass$i($left_cons_f, $right_cons_f) },
           |      c => (CaseClass${i / 2}($left_cons_g), CaseClass${i / 2}($right_cons_g))
           |    )""".stripMargin
      } else {
        val left_Fs     = (1 to i - 1).map(p => s"F$p").mkString(", ")
        val left        = s"""caseclass${i - 1}(X, A, $left_Fs)"""
        val left_cons_f = (1 to i - 1).map(p => s"a._$p").mkString(", ")
        val left_cons_g = (1 to i - 1).map(p => s"c._$p").mkString(", ")

        s"""  implicit def caseclass$i[A, $tparams](implicit X: XFunctor[F], A: Align[F], $Fs): F[CaseClass$i[A, $tparams]] =
           |    X.xmap(A.align($left, F${i}.value))(
           |      { case (a, b) => CaseClass$i($left_cons_f, b) },
           |      c => (CaseClass${i - 1}($left_cons_g), c._$i)
           |    )""".stripMargin
      }
    }
    val sealedtraits = (3 to sum_arity).map { i =>
      // we choose to use induction and call N-1 each time, but it would be
      // equally valid to divide and conquer by splitting the work up evenly
      // between the left and right cases. Divide and conquer has better worst
      // case behaviour at the cost of unintuitive (although lawful) semantics
      // (consider an Eq implementation that may expect to shortcut false from
      // the left only), but produces more code and is slower to compile
      // (especially in Scala 3).
      val tparams  = (1 to i).map(p => s"A$p <: A").mkString(", ")
      val tparams_ = (1 to i).map(p => s"A$p").mkString(", ")
      val Fs       = (1 to i).map(p => s"F$p: Lazy[F[A$p]]").mkString(", ")

      // the casting here works because doing it explicitly we extract and
      // apply using the same data type... the SealedTraitX hierarchy hides
      // the fact that a ._N concrete impl can always be cast to a
      // SealedTrait(N+).
      //
      // not only is this faster at runtime, but it also speeds up the
      // compile.
      val left_Fs       = (1 to i - 1).map(p => s"F$p").mkString(", ")
      val left          = s"""sealedtrait${i - 1}(X, D, $left_Fs)"""
      val left_tparams_ = (1 to i - 1).map(p => s"A$p").mkString(", ")

      s"""  implicit def sealedtrait$i[A, $tparams](implicit X: XFunctor[F], D: Decide[F], $Fs): F[SealedTrait$i[A, $tparams_]] =
         |    X.xmap(D.decide($left, F${i}.value))(
         |      {
         |        case Left(v) => v.asInstanceOf[SealedTrait$i[A, $tparams_]]
         |        case Right(v) => SealedTrait._$i(v)
         |      },
         |      {
         |        case SealedTrait._$i(v) => Right(v)
         |        case other => Left(other.asInstanceOf[SealedTrait${i - 1}[A, $left_tparams_]])
         |      }
         |    )""".stripMargin

    }

    val tuples = (3 to 22).map { i =>
      val As  = (1 to i).map(p => s"A$p").mkString(", ")
      val Fs  = (1 to i).map(p => s"F$p: Lazy[F[A$p]]").mkString(", ")
      val Fs_ = (1 to i).map(p => s"F$p").mkString(", ")
      val vs  = (1 to i).map(p => s"v._$p").mkString(", ")

      s"""  implicit def tuple$i[$As](implicit X: XFunctor[F], A: Align[F], $Fs): F[Tuple$i[$As]] = {
         |    val from = (v: CaseClass$i[Nothing, $As]) => Tuple$i($vs)
         |    val to = (v: Tuple$i[$As]) => CaseClass$i($vs)
         |    X.xmap(caseclass$i(X, A, $Fs_))(from, to)
         |  }"""
    }

    s"""package org.finos.morphir.meta
       |
       |private[meta] trait DerivableGenerated[F[_]] {
       |  implicit def caseclass1[A, A1](implicit X: XFunctor[F], F1: Lazy[F[A1]]): F[CaseClass1[A, A1]] =
       |     X.xmap(F1.value)(t => CaseClass1(t), c => c._1)
       |
       |  implicit def caseclass2[A, A1, A2](implicit X: XFunctor[F], A: Align[F], F1: Lazy[F[A1]], F2: Lazy[F[A2]]): F[CaseClass2[A, A1, A2]] =
       |    X.xmap(A.align(F1.value, F2.value))(t => CaseClass2(t._1, t._2), c => (c._1, c._2))
       |
       |${caseclasses.mkString("\n\n")}
       |
       |  implicit def sealedtrait1[A, A1 <: A](implicit X: XFunctor[F], F1: Lazy[F[A1]]): F[SealedTrait1[A, A1]] =
       |    X.xmap(F1.value)(
       |      v => SealedTrait._1(v),
       |      { case SealedTrait._1(v) => v }
       |    )
       |
       |  implicit def sealedtrait2[A, A1 <: A, A2 <: A](implicit X: XFunctor[F], D: Decide[F], F1: Lazy[F[A1]], F2: Lazy[F[A2]]): F[SealedTrait2[A, A1, A2]] =
       |    X.xmap(D.decide(F1.value, F2.value))(
       |      {
       |        case Right(v) => SealedTrait._2(v)
       |        case Left(v) => SealedTrait._1(v)
       |      },
       |      {
       |        case SealedTrait._2(v) => Right(v)
       |        case SealedTrait._1(v) => Left(v)
       |      }
       |    )
       |
       |${sealedtraits.mkString("\n\n")}
       |
       |  implicit def tuple1[A1](implicit X: XFunctor[F], F1: Lazy[F[A1]]): F[Tuple1[A1]] =
       |     X.xmap(F1.value)(t => Tuple1(t), c => c._1)
       |
       |  implicit def tuple2[A1, A2](implicit A: Align[F], F1: Lazy[F[A1]], F2: Lazy[F[A2]]): F[Tuple2[A1, A2]] =
       |    A.align(F1.value, F2.value)
       |
       |${tuples.mkString("\n\n")}
       |
       |  implicit def either[A1, A2](implicit D: Decide[F], F1: Lazy[F[A1]], F2: Lazy[F[A2]]): F[Either[A1, A2]] =
       |    D.decide(F1.value, F2.value)
       |}""".stripMargin
  }
}
