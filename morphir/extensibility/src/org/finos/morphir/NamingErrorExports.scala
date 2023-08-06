package org.finos.morphir

private[morphir] trait NamingErrorExports {
  sealed case class ParserError(msg: String) extends Exception

}
