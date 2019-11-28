package ru.primetalk.tetris

trait Configuration {
  val width = 10
  val height = 20
}
/**
 *
 * The rules of tetris (https://en.wikipedia.org/wiki/Tetris, https://ru.wikipedia.org/wiki/Тетрис):
 *
 * Tetris is primarily composed of a field of play in which pieces of different geometric forms, called "tetriminos",
 * descend from the top of the field.[9] During this descent, the player can move the pieces laterally and rotate them
 * until they touch the bottom of the field or land on a piece that had been placed before it.[10]
 * The player can neither slow down the falling pieces nor stop them, but can accelerate them in most versions.[11][12]
 * The objective of the game is to use the pieces to create as many horizontal lines of blocks as possible.
 * When a line is completed, it disappears, and the blocks placed above fall one rank.[10] Completing lines grants points,
 * and accumulating a certain number of points moves the player up a level, which increases the number of points
 * granted per completed line.[13] In most versions, the speed of the falling pieces increases with each level,
 * leaving the player with less time to think about the placement.[10] The player can clear multiple lines at once,
 * which can earn bonus points in some versions.[9] It is possible to complete up to four lines simultaneously with
 * the use of the I-shaped tetrimino; this move is called a "Tetris", and is the basis of the game's title.[13]
 * If the player cannot make the blocks disappear quickly enough, the field will start to fill, and when the pieces
 * reach the top of the field and prevent the arrival of additional pieces, the game ends.[10]
 * At the end of each game, the player receives a score based on the number of lines that have been completed.[13]
 * The game never ends with the player's victory; the player can only complete as much lines as possible before an inevitable loss.[9]
 * All of the Tetriminos can fill and clear both singles and doubles. I, J, and L are able to clear triples.
 * Only the I Tetrimino has the capacity to clear four lines simultaneously, and this is referred to as a "tetris".
 * (This may vary depending on the rotation and compensation rules of each specific Tetris implementation.
 * For instance, in the Super Rotation System used in most recent implementations,[14] certain situations allow
 * T, S, and Z to 'snap' into tight spots and clear triples.)[15]
 * Ru: Случайные фигурки тетрамино падают сверху в прямоугольный стакан шириной 10 и высотой 20 клеток.
 * В полёте игрок может поворачивать фигурку на 90° и двигать её по горизонтали.
 * Также можно «сбрасывать» фигурку, то есть ускорять её падение, когда уже решено, куда фигурка должна упасть.
 * Фигурка летит до тех пор, пока не наткнётся на другую фигурку либо на дно стакана.
 * Если при этом заполнился горизонтальный ряд из 10 клеток, он пропадает и всё, что выше него, опускается на одну клетку.
 * Дополнительно показывается фигурка, которая будет следовать после текущей — это подсказка, которая позволяет игроку
 * планировать действия. Темп игры постепенно ускоряется. Игра заканчивается, когда новая фигурка не может поместиться в стакан.
 * Игрок получает очки за каждый заполненный ряд, поэтому его задача — заполнять ряды, не заполняя сам стакан (по вертикали)
 * как можно дольше, чтобы таким образом получить как можно больше очков.
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
    val values = List(I, J, L, O, S, T, Z)
  }

  // We save only 3 points of 4. The position 0,0 is always present.
  case class TetriminoShape(points: List[P])
  val shapes: Map[Tetrimino, TetriminoShape] = Map(
    Tetrimino.I -> TetriminoShape(List( 0~ 2, 0~  1,  0~ -1)),
    Tetrimino.J -> TetriminoShape(List( 0~ 2, 0~  1, -1~  0)),
    Tetrimino.L -> TetriminoShape(List( 0~ 2, 0~  1,  1~  0)),
    Tetrimino.O -> TetriminoShape(List( 0~ 1, 1~  1,  1~  0)),
    Tetrimino.S -> TetriminoShape(List(-1~ 0, 0~ -1,  1~ -1)),
    Tetrimino.T -> TetriminoShape(List(-1~ 0, 1~  0,  0~ -1)),
    Tetrimino.Z -> TetriminoShape(List( 1~ 0, 0~ -1, -1~ -1))
  )

  implicit class TetriminoShapeOps(shape: TetriminoShape) {
    def rotateBy(m: Rotation): TetriminoShape =
      TetriminoShape(shape.points.map(m(_)))
  }

  // case class Color() - home exercise

  // Time that is measured in milliseconds
  type TimeMs = Int

  // There are only 4 angles and they all are connected by rotations.
  type Angle = Rotation

  val defaultTPerYRow: TimeMs = 1000
  /**
   * Part of state about the moving tetrimino.
   * @param position position
   * @param tPerYRow - time that it takes to move to the next row. It's an inverse of the velocity
   *                 we can use this time as an argument to timer.
   */
  case class MovingState(tetrimino: Tetrimino, position: P, angle: Angle, tPerYRow: TimeMs)

  sealed trait CellInfo
  case object EmptyCell extends CellInfo
  // Home exercise: add a logical `color` to match colors of tetriminos
  case class FilledCell(/* color: Color */) extends CellInfo

  // For curious: there is a way to represent list of length N in Scala. Like `List[10, CellInfo]`
  type Row = List[CellInfo]
  type Rows = List[Row]
  val emptyRow: Row = List.fill(width)(EmptyCell)

  /** RowsShape contains a few rows with some filled cells.
   * This data structure is reused for the board itself and for the moving set of rows. */
  case class RowsShape(top: Int, rows: Rows) {
    def bottom: Int = top - rows.size + 1
    def height: Int = top
  }

  /**
   * Board represents the current state of the board.
   * We store only filled rows starting from the top one.
   */
  type Board = RowsShape

  def Board(h: Int, rows: Rows): Board = RowsShape(h, rows)

  def validate(rowsShape: RowsShape): Option[RowsShape] =
    if(rowsShape.bottom <= 0)
      None
    else
      Some(rowsShape)

  // all points in this list should be on the same height
  def listOfPointsToRow(lst: List[P]): Option[Row] = {
    val is = lst.map(_.i).toSet
    if(is.contains(-1) || is.contains(width)) // prevent moving outside left and right borders
      None
    else
      Some(
        (0 until width).toList.map {
          case i if is.contains(i) => FilledCell()
          case _ => EmptyCell
        }
      )
  }

  /** This method "renders" the moving state of the tetrimino in the form of a few rows.
   * If the moving state cannot be rendered within the boundaries, then returns None
   * */
  def convertTetriminoStateToRows(s: MovingState): Option[RowsShape] = {
    val shape = shapes(s.tetrimino)
    val rotatedShape = shape.rotateBy(s.angle)
    val pointsWithZero = ZeroP :: rotatedShape.points

    val pointsShifted = pointsWithZero.map(_ + s.position)
    val rowCells:List[(Int,Option[Row])] = pointsShifted.groupBy(_.j).toList.sortBy(-_._1)
      .map{ case (j,ps) => (j, listOfPointsToRow(ps))}
    if(rowCells.last._1 < 0)
      None
    else
      Some(RowsShape(top = rowCells.head._1, rowCells.map(_._2.get)))
  }

  /** Checks two rows if they collide at some position.
   * it recursively checks the heads of both rows. If one of those
   * is empty - proceed with the rest of the list.
   * if one of the lists finishes, then there is no collision.
   * otherwise - it's collision
   * */
  def isRowsCollision(r1: Row, r2: Row): Boolean =
    (r1, r2 ) match {
      case (EmptyCell::t1, _::t2) => isRowsCollision(t1,t2)
      case (_::t1, EmptyCell::t2) => isRowsCollision(t1,t2)
      case (Nil, _) => false
      case (_, Nil) => false
      case _ => true
    }

  def mergeLists[A](l1: List[A], l2: List[A])(mergeElements: (A, A) => A): List[A] = {
    @scala.annotation.tailrec
    def loop(l1: List[A], l2: List[A], res: List[A] = Nil): List[A] =
      (l1, l2) match {
        case (Nil, _) => res.reverse // we ignore the rest of the second list if it's longer
        case (h1::t1, h2::t2) => loop(t1, t2, mergeElements(h1, h2) :: res)
        case (rest, Nil) => res reverse_::: rest
      }
    loop(l1, l2)
  }

  /** merges two rows */
  def mergeRows(r1: Row, r2: Row, res: Row = Nil): Row =
    mergeLists(r1, r2){
      case (EmptyCell, h) => h
      case (h, _) => h
    }

  def prepend[A](n: Int, a: A)(lst: List[A]): List[A] =
    if(n <= 0) lst else prepend(n-1,a)(a::lst)

  // isThereACollision checks if there is a collision between the board and the moving state.
  def isThereACollision(board: Board, moving: RowsShape): Boolean = {
    val height = math.max(board.height, moving.top)
    val boardRows  = List.fill(height - board.height)(emptyRow) ::: board.rows
    val movingRows = List.fill(height - moving.top)(emptyRow) ::: moving.rows

    boardRows.zip(movingRows).exists((isRowsCollision _).tupled)
  }

  // when we cannot move further, we bake the tetramino at the position
  def bake(board: Board, moving: RowsShape): Board = {
    val height = math.max(board.height, moving.top)
    // make rows to be of the same height
    val boardRows  = List.fill(height - board.height)(emptyRow) ::: board.rows
    val movingRows = List.fill(height - moving.top)(emptyRow) ::: moving.rows
    Board(height, mergeLists(boardRows, movingRows)(mergeRows(_,_)))
  }

  def randomTetrimino(random: Int): Tetrimino =
    Tetrimino.values(math.abs(random) % Tetrimino.values.size)

  def generateMovingState(t: Tetrimino, random: Int): MovingState = {
    val angle = rotations(math.abs(random) % rotations.size)
    MovingState(t, P(width / 2, height - 1), angle, defaultTPerYRow)
  }

  def isFilled(r: Row): Boolean =
    !r.contains(EmptyCell)

  def removeFilledRows(board: Board): (Board, Int) = {
    val (removed, rows) = board.rows.partition(isFilled)
    val count = removed.size
    (Board(board.height - count, rows), count)
  }
}
