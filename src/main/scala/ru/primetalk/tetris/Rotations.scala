package ru.primetalk.tetris

/**
 * This trait contains some basic operations that allow
 * rotations of the vectors.
 */
trait Rotations {

  /**
   * P is a position, a pair, or a vector.
   * We use letters i,j to distinguish logical coordinates measured in cells from
   * viewpoint coordinates x,y measured in pixels.
   * @param i - x
   * @param j - y
   */
  case class P(i: Int, j: Int)

  val ZeroP = P(0, 0)

  implicit class POps(p: P) {
    def +(o: P): P = P(p.i + o.i, p.j + o.j)
    def -(o: P): P = P(p.i - o.i, p.j - o.j)
  }
  /** It's a matrix:
   *  /     \
   *  | a b |
   *  | c d |
   *  \     /
   * In our case all elements are in the range of [-1,1]
   */
  case class Rotation(
    a: Int = 0, b: Int,
    c: Int, d: Int = 0
  )

  // Here is the group of rotations by 90 degrees:
  val rotateRight = Rotation( 0, 1,-1, 0)
  val rotateLeft  = Rotation( 0,-1, 1, 0)
  val rotateId    = Rotation( 1, 0, 0, 1)
  val rotate180   = Rotation(-1, 0, 0,-1)

  val rotations = List(rotateId, rotateRight, rotate180, rotateLeft)

  implicit class RotationOps(rot: Rotation) {
    def apply(p: P): P = P(
      i = rot.a*p.i + rot.b*p.j,
      j = rot.c*p.i + rot.d*p.j
    )
    def apply(other: Rotation): Rotation = Rotation(
      a = rot.a*other.a+rot.b*other.c, b = rot.a*other.b+rot.b*other.d,
      c = rot.c*other.a+rot.d*other.c, d = rot.c*other.b+rot.d*other.d
    )
  }
}
