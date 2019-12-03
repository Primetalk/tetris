package ru.primetalk.tetris

import Game._
import GameState._
import org.scalajs.dom
import org.scalajs.dom.ext.Color

/** Renders board on canvas.
 * It is itself immutable, but performs mutations on the provided context.
 *  TODO: show next tetrimino
 *  TODO*: next tetrimino should rotate at the same speed.
 *  TODO: draw cells with a nice border
 *  TODO: draw original tetris colors
 */
class View(val ctx: dom.CanvasRenderingContext2D) {
  val cellSize: TimeMs = math.min(ctx.canvas.width / Game.width, ctx.canvas.height / Game.height)

  val backgroundColor: Color = dom.ext.Color.White

  def jToY(j: Int): Int = ctx.canvas.height - cellSize * (j + 1)

  def rowView(j: Int, row: Row, color: dom.ext.Color): Unit = {
    row.zipWithIndex.foreach{
      case (CellInfo.FilledCell(_), i) =>
        ctx.fillStyle = color.toString()
        ctx.fillRect(i * cellSize, jToY(j), cellSize - 1, cellSize - 1) // -1 - is the thin line between cells
      case _ => // do nothing for empty cells
    }
  }

  def rowsShapeView(rowsShape: RowsShape, color: dom.ext.Color): Unit = {
    rowsShape.rows.zip((rowsShape.top - 1).to(0, -1)).foreach{ case (r, j) =>
      rowView(j, r, color)
    }
  }

  def redrawGame(oldGame: GameState, newState: GameState): Unit = {
    ctx.beginPath()
    oldGame match {
      case Running(board, _, rowsShape, _) =>
        rowsShapeView(board, backgroundColor)
        rowsShapeView(rowsShape, backgroundColor)
      case Paused(Running(board, _, rowsShape, _)) =>
        rowsShapeView(board, backgroundColor)
        rowsShapeView(rowsShape, backgroundColor)
      case _ =>
    }
    newState match {
      case Running(board, _, rowsShape, _) =>
        rowsShapeView(board, dom.ext.Color.Cyan)
        rowsShapeView(rowsShape, dom.ext.Color.Magenta)
      case Paused(Running(board, _, rowsShape, _)) =>
        rowsShapeView(board, dom.ext.Color.Blue)
        rowsShapeView(rowsShape, dom.ext.Color.Green)
      case _ =>
    }
    ctx.stroke()
  }
}
