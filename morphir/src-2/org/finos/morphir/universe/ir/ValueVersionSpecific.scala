package org.finos.morphir.universe.ir

trait ValueVersionSpecific {
  final type Parameter[+TA, +VA] = (Name, VA, Type[TA])
  object Parameter {
    def apply[TA, VA](name: Name, attributes: VA, tpe: Type[TA]): Parameter[TA, VA] = (name, attributes, tpe)
    def unapply[TA, VA](p: Parameter[TA, VA]): Some[(Name, VA, Type[TA])]           = Some(p)

  }

  implicit class ParameterOps[TA, VA](val p: Parameter[TA, VA]) {
    def mapValueAttributes[VB](f: VA => VB): Parameter[TA, VB] = (p._1, f(p._2), p._3)
    def mapTypeAttributes[TB](f: TA => TB): Parameter[TB, VA]  = (p._1, p._2, p._3.map(f))
    def name: Name                                             = p._1
    def attributes: VA                                         = p._2
    def tpe: Type[TA]                                          = p._3
  }

  final type SpecParameter[+A] = (Name, Type[A])
  object SpecParameter {
    def apply[A](name: Name, tpe: Type[A]): SpecParameter[A]   = (name, tpe)
    def unapply[A](p: SpecParameter[A]): Some[(Name, Type[A])] = Some(p)
  }
  implicit class SpecParameterOps[A](val param: SpecParameter[A]) {
    def map[B](f: A => B): SpecParameter[B] = (param._1, param._2.map(f))
    @inline def name: Name                  = param._1
    @inline def tpe: Type[A]                = param._2
  }
}
