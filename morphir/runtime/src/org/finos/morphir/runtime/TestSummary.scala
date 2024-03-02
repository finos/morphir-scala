package org.finos.morphir.runtime

case class TestSummary(
    message: String,
    success: Boolean // How should Incomplete be handled?
    
)

case class TestResultCounts(passed : Int, failed : Int, skipped : Int, todo : Int)
