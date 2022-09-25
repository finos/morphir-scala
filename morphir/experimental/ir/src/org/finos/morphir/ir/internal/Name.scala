package org.finos.morphir.ir.internal

object Name:
  val empty: Name = List.empty

  opaque type Token = String
  object Token:
    def apply(str: String) = str

  opaque type Name = List[Token]

  given Conversion[Token, String] with
    def apply(token: Token): String = token

  extension (self: Name)
    def toList: List[String] = self.toList.map(_.toString)
    def tokens: List[Token]  = self
    def isEmpty: Boolean     = self.isEmpty

  // object Name:
  //   def apply(first: Token, rest: Token*): Name = first :: rest.toList
  //   def apply(tokens: ::[Token])                = tokens
