package ru.primetalk.tetris

import org.specs2._
import ru.primetalk.tetris.Tetris._
import ru.primetalk.tetris.TetrisGens._

class RotationsTest extends Specification with ScalaCheck { def is = s2"""
  left * right is identity  ${prop {(r: P) => rotateLeft(rotateRight(r)) must be equalTo r }}
  """

}
