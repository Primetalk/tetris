package ru.primetalk.tetris

import org.scalacheck.{Arbitrary, Gen}
import Tetris._

trait TetrisGens {
  implicit def arbitraryRelativePosition: Arbitrary[P] =
    Arbitrary { for {
      i <- Gen.choose(-100, 100)
      j <- Gen.choose(-100, 100)
      } yield P(i, j)
    }

}

object TetrisGens extends TetrisGens
