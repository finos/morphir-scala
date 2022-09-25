package org.finos.morphir.ir
import visitor.MorphirVisitor
object parser:
  given MIRFileParser = new MIRFileParser

  class MIRFileParser:
    def parse[TA, VA, A](input: String, visitor: MorphirVisitor[TA, VA, A]): A = ???

  def parseMIRFile[TA, VA, A](input: String, visitor: MorphirVisitor[TA, VA, A])(using parser: MIRFileParser): A =
    parser.parse(input, visitor)
