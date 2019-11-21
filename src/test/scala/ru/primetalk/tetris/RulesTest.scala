package ru.primetalk.tetris

import org.specs2._
import specification._
import Tetris._
import org.scalacheck._
import TetrisGens._

class RulesTest extends Specification with ScalaCheck { def is = s2"""
  left * right is identity  ${prop {(r: P) => rotateLeft(rotateRight(r)) must be equalTo r }}
  """

//  val p1 = 3 ~ 4
//  val p2 = rotateLeft(p1)
//  val p11 = rotateRight(p2)
//
//  def leftThenRightIsIdentity = {
//    p11 must be equalTo p1
//  }
}
