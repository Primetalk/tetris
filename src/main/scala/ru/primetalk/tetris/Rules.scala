package ru.primetalk.tetris

trait Configuration {
  val width = 10
  val height = 20
}
/**
 *
 * The rules of tetris (https://en.wikipedia.org/wiki/Tetris, https://ru.wikipedia.org/wiki/Тетрис):
 *
 * Tetris is primarily composed of a field of play in which pieces of different geometric forms, called "tetriminos", descend from the top of the field.[9] During this descent, the player can move the pieces laterally and rotate them until they touch the bottom of the field or land on a piece that had been placed before it.[10] The player can neither slow down the falling pieces nor stop them, but can accelerate them in most versions.[11][12] The objective of the game is to use the pieces to create as many horizontal lines of blocks as possible. When a line is completed, it disappears, and the blocks placed above fall one rank.[10] Completing lines grants points, and accumulating a certain number of points moves the player up a level, which increases the number of points granted per completed line.[13] In most versions, the speed of the falling pieces increases with each level, leaving the player with less time to think about the placement.[10] The player can clear multiple lines at once, which can earn bonus points in some versions.[9] It is possible to complete up to four lines simultaneously with the use of the I-shaped tetrimino; this move is called a "Tetris", and is the basis of the game's title.[13] If the player cannot make the blocks disappear quickly enough, the field will start to fill, and when the pieces reach the top of the field and prevent the arrival of additional pieces, the game ends.[10] At the end of each game, the player receives a score based on the number of lines that have been completed.[13] The game never ends with the player's victory; the player can only complete as much lines as possible before an inevitable loss.[9]
 * All of the Tetriminos can fill and clear both singles and doubles. I, J, and L are able to clear triples. Only the I Tetrimino has the capacity to clear four lines simultaneously, and this is referred to as a "tetris". (This may vary depending on the rotation and compensation rules of each specific Tetris implementation. For instance, in the Super Rotation System used in most recent implementations,[14] certain situations allow T, S, and Z to 'snap' into tight spots and clear triples.)[15]
 * Ru: Случайные фигурки тетрамино падают сверху в прямоугольный стакан шириной 10 и высотой 20 клеток.
 * В полёте игрок может поворачивать фигурку на 90° и двигать её по горизонтали.
 * Также можно «сбрасывать» фигурку, то есть ускорять её падение, когда уже решено, куда фигурка должна упасть.
 * Фигурка летит до тех пор, пока не наткнётся на другую фигурку либо на дно стакана.
 * Если при этом заполнился горизонтальный ряд из 10 клеток, он пропадает и всё, что выше него, опускается на одну клетку. Дополнительно показывается фигурка, которая будет следовать после текущей — это подсказка, которая позволяет игроку планировать действия. Темп игры постепенно ускоряется. Игра заканчивается, когда новая фигурка не может поместиться в стакан. Игрок получает очки за каждый заполненный ряд, поэтому его задача — заполнять ряды, не заполняя сам стакан (по вертикали) как можно дольше, чтобы таким образом получить как можно больше очков.
 */
trait Rules extends Configuration with Rotations {
  sealed trait Tetrimino
  object Tetrimino {
    case object I extends Tetrimino
    case object J extends Tetrimino
    case object L extends Tetrimino
    case object O extends Tetrimino
    case object S extends Tetrimino
    case object T extends Tetrimino
    case object Z extends Tetrimino
  }
  // We save only 3 points. The position 0,0 is always present.
  case class TetriminoShape(points: List[P])
  val shapes: Map[Tetrimino, TetriminoShape] = Map(
    Tetrimino.I -> TetriminoShape(List(0~ 2, 0~1, 0~ -1)),
    Tetrimino.J -> TetriminoShape(List(0~ 2, 0~1, -1~0)),
    Tetrimino.L -> TetriminoShape(List(0~ 2, 0~1, 1~0)),
    Tetrimino.O -> TetriminoShape(List(0~ 1, 1~1, 1~0)),
    Tetrimino.S -> TetriminoShape(List(-1~ 0, 0~ -1, 1~ -1)),
    Tetrimino.T -> TetriminoShape(List(-1~ 0, 1~ 0, 0~ -1)),
    Tetrimino.Z -> TetriminoShape(List(1~ 0, 0~ -1, -1~ -1))
  )


  sealed trait Control
  object Control {
    case object Left extends Control
    case object Right extends Control
    case object RotateLeft extends Control
    case object RotateRight extends Control
    case object Drop extends Control
  }


  // case class Color() - home exercise

  // Time that is measured in milliseconds
  type TimeMs = Int

  // There are only 4 angles and they all are connected by rotations.
  type Angle = Rotation
  /**
   * Part of state about the moving tetrimino.
   * @param x vertical position
   * @param y vertical position
   * @param tPerYRow - time that it takes to move to the next row. It's an inverse of the velocity
   *                 we can use this time as an argument to timer.
   */
  case class MovingState(tetrimino: Tetrimino, x: Int, y: Int, angle: Angle, tPerYRow: TimeMs)

  sealed trait CellInfo
  case object EmptyCell extends CellInfo
  case class FilledCell(/* color: Color */) extends CellInfo

  type Row = List[CellInfo]
  val emptyRow: Row = List.fill(width)(EmptyCell)
  /**
   * Board represents the current state of the board.
   * We store only filled rows starting from the top one.
   */
  case class Board(height: Int, rows: List[Row])

  /** RowsShape contains the moving part of the board. */
  case class RowsShape(top: Int, rows: List[Row])
  /** This method "renders" the moving state of the tetrimino in the form of a few rows. */
  def convertTetriminoStateToRows(s: MovingState): RowsShape = ???

  /** Checks two rows if they collide at some position. */
  def isRowsCollision(r1: Row, r2: Row): Boolean = {
    r1.nonEmpty && ((r1, r2 ) match {
      case (EmptyCell::t1, _::t2) => isRowsCollision(t1,t2)
      case (_::t1, EmptyCell::t2) => isRowsCollision(t1,t2)
      case _ => true
    })
  }
  def prepend[A](n: Int, a: A)(lst: List[A]): List[A] =
    if(n <= 0) lst else prepend(n-1,a)(a::lst)

  // isThereACollision checks if there is a collision between the board and the moving state.
  def isThereACollision(board: Board, movingState: MovingState): Boolean = {
    val moving = convertTetriminoStateToRows(movingState)
    prepend(moving.top - board.height, emptyRow)(board.rows).zip(
      prepend(- moving.top + board.height, emptyRow)(moving.rows)
    ).exists((isRowsCollision _).tupled)
  }

  // when we cannot move further, we bake the tetramino at the position
  def bake(board: Board, tetrimino: Tetrimino, movingState: MovingState): Board = ???
}
