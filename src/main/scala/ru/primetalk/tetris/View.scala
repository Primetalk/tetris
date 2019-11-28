package ru.primetalk.tetris

import Game._
import org.scalajs.dom
import org.scalajs.dom.ext.Color

/** Renders just the board on canvas. */
class View(val ctx: dom.CanvasRenderingContext2D) {
  val cellSize = math.min(ctx.canvas.width / Game.width, ctx.canvas.height / Game.height)

  val backgroundColor = dom.ext.Color.White

  def jToY(j: Int): Int = ctx.canvas.height - cellSize * (j + 1)

  def rowView(j: Int, row: Row, color: dom.ext.Color): Unit = {
    row.zipWithIndex.filter(_._1.isInstanceOf[FilledCell]).map(_._2).foreach{
      i =>
        ctx.fillStyle = color.toString()
        ctx.fillRect(i * cellSize, jToY(j), cellSize - 1, cellSize - 1)
    }
  }

  def boardView(board: Board): Unit = {
    rowsShapeView(board, dom.ext.Color.Cyan)
  }

  def rowsShapeView(rowsShape: RowsShape, color: dom.ext.Color): Unit = {
    rowsShape.rows.zip((rowsShape.top - 1).to(0, -1)).foreach{ case (r, j) =>
      rowView(j, r, color)
    }
  }

  def redrawGame(oldGame: State, newState: State): Unit = {
    ctx.beginPath()
    oldGame match {
      case RunningGameState(board, _, rowsShape, _) =>
        rowsShapeView(board, backgroundColor)
        rowsShapeView(rowsShape, backgroundColor)
      case _ =>

    }
    newState match {
      case RunningGameState(board, _, rowsShape, _) =>
        boardView(board)
        rowsShapeView(rowsShape, dom.ext.Color.Magenta)
      case _ =>
    }
    ctx.stroke()
  }
}
