package morphir.mir

final case class Attributed[Case[+_], A](caseValue: Case[Attributed[Case, A]], attributes: A)
