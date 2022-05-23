package main.marsrover.internals

import main.marsrover.internals.domain.{Coordinates, Row}


package object domain {
  type Row = Vector[Coordinates]

  case class Coordinates(
                          x: Int,
                          y: Int
                        )

  case class DisplayInput(
                           grid: Grid
                         )
}


case class Instructions(input: String){
    def parse(inputString: String = input): Seq[Char] = {
      inputString.toSeq
    }
  }
