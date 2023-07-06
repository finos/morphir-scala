package org.finos.morphir.testing.munit

trait MorphirTestSuite extends munit.ScalaCheckSuite {  
  def describe(name:String)(thunk: => Unit): Unit = {
    val countBefore = munitTestsBuffer.size
    val _ = thunk
    val countAfter = munitTestsBuffer.size
    val countRegistered = countAfter - countBefore
    val registered = munitTestsBuffer.toList.drop(countBefore)
    (0 until countRegistered).foreach(_ => munitTestsBuffer.remove(countBefore))
    registered.foreach(t => munitTestsBuffer += t.withName(s"$name - ${t.name}"))
  }

  /**
    * An alias for `describe`.    
    */
  def suite(name:String)(thunk: => Unit): Unit = describe(name)(thunk)
}
