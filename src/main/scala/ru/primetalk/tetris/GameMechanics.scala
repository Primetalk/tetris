package ru.primetalk.tetris

trait GameMechanics extends Rules {

  sealed trait State
  case class RunningGameState(board: Board, movingState: MovingState, rowsShape: RowsShape, nextTetrimino: Tetrimino) extends State
  case class FinishedGame(board: Board) extends State
  case class PausedGame(s: State) extends State

  sealed trait Control
  object Control {
    case class ShiftXBy(deltaX: Int) extends Control
    case class RotateBy(m: Rotation) extends Control
    case object Drop extends Control
  }

  sealed trait Event
  case object Timer extends Event
  case class UserInteraction(control: Control) extends Event
  case object Pause extends Event

  def move(movingState: MovingState, rowsShape: RowsShape, event: Event): (MovingState, Option[RowsShape]) = event match {
    case Timer =>
      (
        movingState.copy(position = movingState.position + P(0, -1)),
        validate(rowsShape.copy(top = rowsShape.top - 1))
      )
    case Pause => (movingState, Some(rowsShape))
    case UserInteraction(Control.Drop) => // we just move by 1 down
      (
        movingState.copy(position = movingState.position + P(0, -1)),
        validate(rowsShape.copy(top = rowsShape.top - 1))
      )
    case UserInteraction(Control.ShiftXBy(deltaX)) =>
      val nextMovingState = movingState.copy(position = movingState.position + P(deltaX, 0))
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

  def handleEvent(s: State, event: Event, random: Int): State = (s, event) match {
    case (f: FinishedGame, _) => f
    case (PausedGame(g), Pause) => g
    case (PausedGame(_), _) => s // ignoring other events when paused
    case (_, Pause) => PausedGame(s)
    case (RunningGameState(board, movingState, rowsShape, nextTetrimino), _) =>
      val (nextMovingState, nextRowsShapeOpt) = move(movingState, rowsShape, event)
      def takeNextTetrimino: State = {
        val bakedBoard = bake(board, rowsShape)
        val (nextBoard, count) = removeFilledRows(bakedBoard)
        val preMovingState = generateMovingState(nextTetrimino, random)
        val nextMovingState = preMovingState.copy(tPerYRow = preMovingState.tPerYRow - count)
        val nextNextTetrimino = randomTetrimino(random)
        val nextRowsShape2 = convertTetriminoStateToRows(nextMovingState)
        if(nextRowsShape2.isEmpty) {
          println("Couldn't convert just generated Tetrimino")
        }
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
          case Pause =>
            RunningGameState(board, movingState, rowsShape, nextTetrimino)
        }
      }
  }

  def startGame(random: Int): RunningGameState = {
    val t = randomTetrimino(random)
    val m = generateMovingState(t, random)
    convertTetriminoStateToRows(m) match {
      case Some(rowsShape) =>
        val initialRows = List(List.fill(width)(FilledCell()))
        RunningGameState (Board(initialRows.size, initialRows), m, rowsShape,
          randomTetrimino (random / Tetrimino.values.size) )
      case None =>
        println("Failed to generate a tetrimino")
        startGame(random + 1)
    }
  }
}
