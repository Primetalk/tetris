package ru.primetalk.tetris

/**
 * This trait contains some basic operations that allow
 * rotations of the vectors.
 */
trait Rotations {

  /**
   * P is a position or vector.
   * @param i - x
   * @param j - y
   */
  case class P(i: Int, j: Int)

  val ZeroP = P(0, 0)
  implicit class IntOps(i: Int){
    def ~(j: Int): P = P(i,j)
  }

  case class Rotation(a: Int = 0, b: Int, c: Int, d: Int = 0) // all elements are in the range of [-1,1]

  // Here is the group of rotations:
  val rotateRight = Rotation( 0, 1,-1, 0)
  val rotateLeft  = Rotation( 0,-1, 1, 0)
  val rotateId    = Rotation( 1, 0, 0, 1)
  val rotate180   = Rotation(-1, 0, 0,-1)
  val rotations = List(rotateId, rotateRight, rotate180, rotateLeft)

  implicit class RotationOps(m: Rotation) {
    def apply(r: P): P = P(
      i = m.a*r.i + m.b*r.j,
      j = m.c*r.i + m.d*r.j
    )
    def apply(m2: Rotation): Rotation = Rotation(
      a = m.a*m2.a+m.b*m2.c, b = m.a*m2.b+m.b*m2.d,
      c = m.c*m2.a+m.d*m2.c, d = m.c*m2.b+m.d*m2.d
    )
  }
}
