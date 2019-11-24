package ru.primetalk.tetris

trait GameStates extends Rules {

  sealed trait State
  case class RunningGameState(board: Board, movingState: MovingState, rowsShape: RowsShape, nextTetrimino: Tetrimino) extends State
  case class FinishedGame(board: Board) extends State

  sealed trait Event
  case object Timer extends Event
  case class UserInteraction(control: Control) extends Event

  def move(movingState: MovingState, rowsShape: RowsShape, event: Event): (MovingState, Option[RowsShape]) = event match {
    case Timer =>
      (
        movingState.copy(y = movingState.y - 1),
        validate(rowsShape.copy(top = rowsShape.top - 1))
      )
    case UserInteraction(Control.Drop) => // we just move by 1 down
      (
        movingState.copy(y = movingState.y - 1),
        validate(rowsShape.copy(top = rowsShape.top - 1))
      )
    case UserInteraction(Control.ShiftXBy(deltaX)) =>
      val nextMovingState = movingState.copy(x = movingState.x + deltaX)
      (
        nextMovingState,
        convertTetriminoStateToRows(nextMovingState)
      )
    case UserInteraction(Control.RotateBy(m)) =>
      val nextMovingState = movingState.copy(angle = m(movingState.angle))
      (
        nextMovingState,
        convertTetriminoStateToRows(nextMovingState)
      )
  }

  def handleEvent(s: State, event: Event, random: Int): State = s match {
    case f: FinishedGame => f
    case RunningGameState(board, movingState, rowsShape, nextTetrimino) =>
      val (nextMovingState, nextRowsShapeOpt) = move(movingState, rowsShape, event)
      def takeNextTetrimino: State = {
        val nextBoard = bake(board, rowsShape)
        val nextMovingState = generateMovingState(nextTetrimino, random)
        val nextNextTetrimino = randomTetrimino(random)
        val nextRowsShape2 = convertTetriminoStateToRows(nextMovingState)
        if(nextRowsShape2.isEmpty || isThereACollision(nextBoard, nextRowsShape2.get))
          FinishedGame(nextBoard)
        else
          RunningGameState(nextBoard, nextMovingState, nextRowsShape2.get, nextNextTetrimino)
      }
      nextRowsShapeOpt match {
        case Some(nextRowsShape) =>
          if(isThereACollision(board, nextRowsShape)) { // cannot move
            takeNextTetrimino
          } else {
            RunningGameState(board, nextMovingState, nextRowsShape, nextTetrimino)
          }
        case None => event match {
          case UserInteraction(_) =>
            RunningGameState(board, movingState, rowsShape, nextTetrimino)
          case Timer =>
            takeNextTetrimino // if we cannot move on timer, we bake the current state and move to the next tetrimino
        }
      }
  }
}
