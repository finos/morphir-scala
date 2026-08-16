package morphir.ui.internal

import kyo.*
import morphir.ui.layout.{ResizeHandle, ShellState}
import scala.scalajs.js

/**
 * Turns pointer drags on the shell's resize strips into store commands.
 *
 * kyo-ui carries no pointer-down/move events at RC6, so this is DOM work: one delegated `mousedown` listener on the
 * document decides which strip was grabbed, then move and up listeners run the matching [[ShellState]] resize command.
 * Delegation rather than per-element binding, because the regions are re-created whenever they collapse or expand. The
 * store still owns the sizes and their bounds; this adapter only reports pointer positions.
 */
private[ui] object PointerResize:

  private val resizingColumn = "resizing-col"
  private val resizingRow    = "resizing-row"

  def attach(state: ShellState): Unit < Sync = Sync.defer {
    import AllowUnsafe.embrace.danger
    val document = js.Dynamic.global.document
    val body     = document.body

    var active       = ""
    var startPointer = 0.0
    var startSize    = 0

    def commit(size: Double): Unit =
      val command = active match
        case ResizeHandle.leftId   => state.resizeLeft(size.round.toInt)
        case ResizeHandle.rightId  => state.resizeRight(size.round.toInt)
        case ResizeHandle.bottomId => state.resizeBottom(size.round.toInt)
        case _                     => Kyo.unit
      Sync.Unsafe.evalOrThrow(command)

    val onMove: js.Function1[js.Dynamic, Unit] = (event: js.Dynamic) =>
      if active.nonEmpty then
        // The left panel grows with the pointer; the right and bottom panels grow against it.
        val delta = active match
          case ResizeHandle.bottomId => startPointer - event.clientY.asInstanceOf[Double]
          case ResizeHandle.rightId  => startPointer - event.clientX.asInstanceOf[Double]
          case _                     => event.clientX.asInstanceOf[Double] - startPointer
        commit(startSize + delta)

    val onUp: js.Function1[js.Dynamic, Unit] = (_: js.Dynamic) =>
      if active.nonEmpty then
        active = ""
        val _ = body.classList.remove(resizingColumn, resizingRow)

    val onDown: js.Function1[js.Dynamic, Unit] = (event: js.Dynamic) =>
      val id = event.target.id.asInstanceOf[js.UndefOr[String]].getOrElse("")
      if id == ResizeHandle.leftId || id == ResizeHandle.rightId || id == ResizeHandle.bottomId then
        active = id
        startSize = id match
          case ResizeHandle.leftId  => state.leftWidth.unsafe.get().px
          case ResizeHandle.rightId => state.rightWidth.unsafe.get().px
          case _                    => state.bottomHeight.unsafe.get().px
        startPointer =
          if id == ResizeHandle.bottomId then event.clientY.asInstanceOf[Double]
          else event.clientX.asInstanceOf[Double]
        val _ = body.classList.add(if id == ResizeHandle.bottomId then resizingRow else resizingColumn)
        // Suppress text selection for the life of the drag.
        val _ = event.preventDefault()

    val _ = document.addEventListener("mousedown", onDown)
    val _ = document.addEventListener("mousemove", onMove)
    val _ = document.addEventListener("mouseup", onUp)
  }
