package ru.primetalk.tetris

trait GameStates extends Rules {

  sealed trait State
  case class RunningGameState(board: Board, movingState: MovingState, rowsShape: RowsShape, nextTetrimino: Tetrimino) extends State
  case class FinishedGame(board: Board) extends State

  sealed trait Event
  case object Timer extends Event
  case class UserInteraction(control: Control) extends Event

  def move(movingState: MovingState, rowsShape: RowsShape, event: Event): (MovingState, RowsShape) = event match {
    case Timer =>
      (
        movingState.copy(y = movingState.y - 1),
        rowsShape.copy(top = rowsShape.top - 1)
      )
    case UserInteraction(Control.Drop) => // we just move by 1 down
      (
        movingState.copy(y = movingState.y - 1),
        rowsShape.copy(top = rowsShape.top - 1)
      )
    case UserInteraction(Control.ShiftXBy(deltaX)) =>
      val nextMovingState = movingState.copy(x = movingState.x + deltaX)
      (
        nextMovingState,
        convertTetriminoStateToRows(nextMovingState)
      )
    case UserInteraction(Control.RotateBy(m)) =>
      val nextAngle = m(movingState.angle)
      val nextMovingState = movingState.copy(angle = nextAngle)
      val nextRows = convertTetriminoStateToRows(nextMovingState)
      (nextMovingState, nextRows)
  }

  def handleEvent(s: State, event: Event, random: Int): State = s match {
    case f: FinishedGame => f
    case RunningGameState(board, movingState, rowsShape, nextTetrimino) =>
      val (nextMovingState, nextRowsShape) = move(movingState, rowsShape, event)

      if(isThereACollision(board, nextRowsShape)) { // cannot move
        val nextBoard = bake(board, rowsShape)
        val nextMovingState = generateMovingState(nextTetrimino, random)
        val nextNextTetrimino = randomTetrimino(random)
        val nextRowsShape2 = convertTetriminoStateToRows(nextMovingState)
        if(isThereACollision(nextBoard, nextRowsShape2))
          FinishedGame(nextBoard)
        else
          RunningGameState(nextBoard, nextMovingState, nextRowsShape, nextNextTetrimino)
      } else {
        RunningGameState(board, nextMovingState, nextRowsShape, nextTetrimino)
      }
  }
}
