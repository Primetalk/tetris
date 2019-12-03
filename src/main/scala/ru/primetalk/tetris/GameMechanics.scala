package ru.primetalk.tetris

import org.scalajs.dom.ext.KeyCode

trait Game[S, Control] {
  def startGame(random: Int): S
  def handleEvent(s: S, e: Event, random: Int): S
  // returns the delay until the next time moment
  def nextTimerEventDelayMs(s: S): Option[Long]

  sealed trait Event
  object Event {
    case object Timer extends Event
    case class MovingShapeControl(control: Control) extends Event
    case object Pause extends Event
  }

  def convertKeyCode(keyCode: Int): Option[Control]
}
/**
 *
 * TODO: Add game score, improve speedup
 * TODO: Add a few randomly filled rows.
 */
trait GameMechanics extends RowOfCells {

  sealed trait GameState
  object GameState {
    case class Running(board: Board, movingState: MovingState, rowsShape: RowsShape, nextTetrimino: Tetrimino) extends GameState
    case class Finished(board: Board) extends GameState
    case class Paused(s: GameState) extends GameState
  }
  import GameState._

  sealed trait Control
  object Control {
    case class ShiftXBy(deltaX: Int) extends Control
    case class RotateBy(m: Rotation) extends Control
    case object Drop extends Control
  }

  def move(movingState: MovingState, rowsShape: RowsShape, control: Control): (MovingState, Option[RowsShape]) = control match {
    case Control.Drop => // we just move by 1 down
      (
        movingState.copy(position = movingState.position + P(0, -1)),
        rowsShape.copy(top = rowsShape.top - 1).makeSureAboveLowerBorder
      )
    case Control.ShiftXBy(deltaX) =>
      val nextMovingState = movingState.copy(position = movingState.position + P(deltaX, 0))
      (
        nextMovingState,
        convertTetriminoStateToRows(nextMovingState)
      )
    case Control.RotateBy(m) =>
      val nextMovingState = movingState.copy(angle = m(movingState.angle))
      (
        nextMovingState,
        convertTetriminoStateToRows(nextMovingState)
      )
  }

  // attempts to start moving the next tetrimino
  def takeNextTetrimino(gameState: GameState.Running, random: Int): GameState = gameState match {
    case Running(board, _, rowsShape, nextTetrimino) =>
      val bakedBoard = bake(board, rowsShape)
      val (nextBoard, count) = removeFilledRows(bakedBoard)
      val preMovingState = generateMovingState(nextTetrimino, random)
      val nextMovingState = preMovingState.copy(tPerYRow = preMovingState.tPerYRow - count)
      val nextNextTetrimino = randomTetrimino(random)
      val nextRowsShape2Opt = convertTetriminoStateToRows(nextMovingState)
      nextRowsShape2Opt match {
        case None =>
          println("Couldn't convert just generated Tetrimino")
          Finished(nextBoard)
        case Some(nextRowsShape2) if (isThereACollision(nextBoard, nextRowsShape2)) =>
          Finished(nextBoard)
        case Some(nextRowsShape2) =>
          Running(nextBoard, nextMovingState, nextRowsShape2, nextNextTetrimino)
      }
  }

  object GameImpl extends Game[GameState, Control] {
    import Event._
    def handleEvent(s: GameState, event: Event, random: Int): GameState = (s, event) match {
      case (f: Finished, _) => f
      case (Paused(g), Pause) => g
      case (Paused(_), _) => s // ignoring other events when paused
      case (_, Pause) => Paused(s)
      case (gameState@Running(board, movingState, rowsShape, nextTetrimino), _) =>
        val (nextMovingState, nextRowsShapeOpt) = event match {
          case MovingShapeControl(control) =>
            move(movingState, rowsShape, control)
          case Timer =>
            move(movingState, rowsShape, Control.Drop)
          case Pause =>
            (movingState, Some(rowsShape))
        }

        nextRowsShapeOpt match {
          case Some(nextRowsShape) =>
            if (isThereACollision(board, nextRowsShape)) { // cannot move inside existing filled cells
              takeNextTetrimino(gameState, random)
            } else {
              Running(board, nextMovingState, nextRowsShape, nextTetrimino)
            }
          case None => event match {
            case Timer =>
              takeNextTetrimino(gameState, random) // if we cannot move on timer, we bake the current state and move to the next tetrimino
            case _ =>
              gameState
          }
        }
    }

    def startGame(random: Int): GameState.Running = {
      val t = randomTetrimino(random)
      val m = generateMovingState(t, random)
      convertTetriminoStateToRows(m) match {
        case Some(rowsShape) =>
          val initialRows = List() // List.fill(width)(FilledCell()))
          GameState.Running(Board(initialRows.size, initialRows), m, rowsShape,
            randomTetrimino(random / Tetrimino.values.size))
        case None =>
          println("Failed to generate a tetrimino")
          startGame(random + 1)
      }
    }

    def nextTimerEventDelayMs(s: GameState): Option[Long] = s match {
      case Running(_, movingState, _, _) => Some(movingState.tPerYRow)
      case Finished(_) => None
      case Paused(_) => None
    }
    def convertKeyCode(keyCode: Int): Option[Control] = keyCode  match {
      case KeyCode.Space => Some(Control.RotateBy(rotateRight))
      case KeyCode.Up    => Some(Control.RotateBy(rotateLeft))
      case KeyCode.Left  => Some(Control.ShiftXBy(-1))
      case KeyCode.Right => Some(Control.ShiftXBy(1))
      case KeyCode.Down  => Some(Control.Drop)
//      case KeyCode.P     => Some(Game.Event.Pause)
      case _ => None
    }

  }
}

object TetrisGame extends GameMechanics
