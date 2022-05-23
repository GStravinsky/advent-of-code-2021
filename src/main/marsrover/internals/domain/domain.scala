package marsrover.internals

package object domain {
  type Row = Vector[GridCoordinates]

  trait Coordinates{
    val x: Int
    val y: Int
  }
  case class GridCoordinates(x:Int, y:Int) extends Coordinates

  case class RoverCoordinates(
                               x: Int,
                               y:Int,
                               direction: Direction
                             ) extends  Coordinates {
    val gridCoordinates = GridCoordinates(x,y)
  }
  trait Display {
    def getInput: Array[String]
    def parseInput(input: Array[String]): Coordinates
    def inputIsValid(input: Array[String]): Boolean
  }

  trait Rover {
    def name: String
    def symbol: Char
  }

  trait Direction
  case object North extends Direction
  case object South extends Direction
  case object East extends Direction
  case object West extends Direction

}


