package org.finos.morphir.universe.ir

trait ValueVersionSpecific:

  final opaque type Parameter[+TA, +VA] <: (Name, VA, Type[TA]) = (Name, VA, Type[TA])
  object Parameter:
    def apply[TA, VA](name: Name, attributes: VA, tpe: Type[TA]): Parameter[TA, VA] = (name, attributes, tpe)
    def unapply[TA, VA](p: Parameter[TA, VA]): Some[(Name, VA, Type[TA])]           = Some(p)

    extension [TA, VA](p: Parameter[TA, VA])
      def mapTypeAttributes[TB](f: TA => TB): Parameter[TB, VA] =
        (p._1, p._2, p._3.map(f))

      def mapValueAttributes[VB](f: VA => VB): Parameter[TA, VB] =
        (p._1, f(p._2), p._3)
      def name: Name     = p._1
      def attributes: VA = p._2
      def tpe: Type[TA]  = p._3

  final opaque type SpecParameter[+A] <: (Name, Type[A]) = (Name, Type[A])

  object SpecParameter:
    def apply[A](name: Name, tpe: Type[A]): SpecParameter[A]   = (name, tpe)
    def unapply[A](p: SpecParameter[A]): Some[(Name, Type[A])] = Some(p)

    extension [A](arg: SpecParameter[A])
      def map[B](f: A => B): SpecParameter[B] = (arg._1, arg._2.map(f))
      def name: Name                          = arg._1
      def tpe: Type[A]                        = arg._2

end ValueVersionSpecific
