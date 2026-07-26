package morphir.langkit.elm.compiler

object SourceOffsets:

  /** Convert 1-based line/column to a 0-based character offset in `source`. */
  def offsetAt(source: String, line: Int, column: Int): Int =
    require(line >= 1 && column >= 1, s"line and column are 1-based, got ($line, $column)")
    var currentLine = 1
    var index       = 0
    while index < source.length && currentLine < line do
      if source.charAt(index) == '\n' then currentLine += 1
      index += 1
    if currentLine != line then source.length
    else
      val lineStart = index
      var col       = 1
      while col < column && index < source.length && source.charAt(index) != '\n' do
        index += 1
        col += 1
      index.min(source.length).max(lineStart)

  def lineColumnAt(source: String, offset: Int): (Int, Int) =
    val bounded = offset.max(0).min(source.length)
    var line    = 1
    var column  = 1
    var index   = 0
    while index < bounded do
      if source.charAt(index) == '\n' then
        line += 1
        column = 1
      else column += 1
      index += 1
    (line, column)
