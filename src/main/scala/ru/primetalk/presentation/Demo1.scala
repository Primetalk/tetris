package ru.primetalk.presentation

import org.scalajs.dom.ext.KeyCode
import ru.primetalk.tetris.Game
import ru.primetalk.tetris.BasicTetrisDefinitions._

// show all shapes
trait Demo1  {

  case class SingleShape(movingState: MovingState, rowsShape: RowsShape)

  sealed trait DemoState
  object DemoState {
    case class Running(shapes: List[SingleShape]) extends DemoState
  }
  import DemoState._

  sealed trait Control
  object Control {
    case class RotateBy(m: Rotation) extends Control
  }


  object GameImpl extends Game[DemoState, Control] {
    import Event._
    def handleEvent(s: DemoState, event: Event, random: Int): DemoState = (s, event) match {
      case (Running(shapes), MovingShapeControl(Control.RotateBy(rotation))) =>
        Running( shapes.
          map { s =>
            val angle2 = rotation(s.movingState.angle)
            val m2 = s.movingState.copy(angle = angle2)
            val rows2 = convertTetriminoStateToRows(m2)
            rows2 match {
              case Some(r) =>
                SingleShape(m2, r)
              case None =>
                s
            }
          }
        )
      case (gameState, _) => gameState
    }

    def startGame(random: Int): DemoState.Running = {
      val ms = Tetrimino.values.zipWithIndex.map{ case (t, i) =>
        MovingState(t, P(width / 3 + (width / 3) * (i % 2), height * i / 8 + 3), rotateId, defaultTPerYRow)
      }
      Running(ms.flatMap(m => convertTetriminoStateToRows(m).map(SingleShape(m, _))))
    }

    def nextTimerEventDelayMs(s: DemoState): Option[Long] = Some(defaultTPerYRow)

    def convertKeyCode(keyCode: Int): Option[Control] = keyCode  match {
      case KeyCode.Space => Some(Control.RotateBy(rotateRight))
      case KeyCode.Up    => Some(Control.RotateBy(rotateLeft))
      case KeyCode.Left  => Some(Control.RotateBy(rotateLeft))
      case KeyCode.Right => Some(Control.RotateBy(rotateRight))
      case _ => None
    }

  }
}

object Demo1 extends Demo1 {

}
