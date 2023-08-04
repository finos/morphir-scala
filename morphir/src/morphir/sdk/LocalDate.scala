package morphir.sdk

object LocalDate {
  type LocalDate = java.time.LocalDate

  def addDays(count: Basics.Int)(date: LocalDate): LocalDate = {
    val countAsLong = count.toLong
    date plusDays countAsLong
  }

  def addWeeks(count: Basics.Int)(date: LocalDate): LocalDate = {
    val countAsLong = count.toLong
    date plusWeeks countAsLong
  }

  def addMonths(count: Basics.Int)(date: LocalDate): LocalDate = {
    val countAsLong = count.toLong
    date plusMonths countAsLong
  }

  def addYears(count: Basics.Int)(date: LocalDate): LocalDate = {
    val countAsLong = count.toLong
    date plusYears countAsLong
  }

}
