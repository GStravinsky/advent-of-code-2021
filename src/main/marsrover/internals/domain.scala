package main.marsrover.internals


package object domain {
  type Row = Vector[Coordinates]

  case class Coordinates(
                          x: Int,
                          y: Int
                        )

  trait Display {
    def getInput: Array[String]
    def parseInput(input: Array[String]): Coordinates
    def inputIsValid(input: Array[String]): Boolean
  }
}


case class Instructions(input: String){
    def parse(inputString: String = input): Seq[Char] = {
      inputString.toSeq
    }
  }
