package ru.primetalk.presentation

import org.scalajs.dom
import ru.primetalk.tetris.{GameView, TetrisViews}
import Demo1._

class Demo1View(val ctx: dom.CanvasRenderingContext2D) extends GameView[DemoState] with TetrisViews {

  import ru.primetalk.tetris.BasicTetrisDefinitions.Tetrimino._
  val colors: Map[ru.primetalk.tetris.BasicTetrisDefinitions.Tetrimino, dom.ext.Color] = Map(
    I -> dom.ext.Color("#00f0f0"),
    J -> dom.ext.Color("#0000f0"),
    L -> dom.ext.Color("#f0a000"),
    O -> dom.ext.Color("#f0f000"),
    S -> dom.ext.Color("#00f000"),
    T -> dom.ext.Color("#a000f0"),
    Z -> dom.ext.Color("#f00000"))
  import DemoState._
  def redrawGame(oldGame: DemoState, newState: DemoState): Unit = {
    ctx.beginPath()
    oldGame match {
      case Running(shapes) =>
        shapes.foreach{ s =>
          rowsShapeView(s.rowsShape, backgroundColor)
        }
    }
    newState match {
      case Running(shapes) =>
        shapes.foreach{ s =>
          val color = colors(s.movingState.tetrimino)
          rowsShapeView(s.rowsShape, color)
        }
    }
    ctx.stroke()
  }
}
