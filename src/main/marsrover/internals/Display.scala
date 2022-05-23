package marsrover.internals

import marsrover.internals.domain.{Coordinates, Display, East, GridCoordinates, North, RoverCoordinates, South, West}

class DisplayGrid extends Display{


  def parseInputAndGetrid(input: Array[String]):Grid = {
    val parsedCoords = parseInput(input)
    Grid(parsedCoords)
  }
  def getInput  = {
    println("Insert grid dimensions in format <Int Int>")
    val input = scala.io.StdIn.readLine()
    inputIsValid(input.split(" ")) match {
      case true => input.split(" ")
      case false => {
        println("The input has to be in format <Int Int>")
        getInput
      }
    }
  }

  override def parseInput(input: Array[String]): GridCoordinates = {
    GridCoordinates(input.head.toInt, input.tail.head.toInt )
  }

  def inputIsValid(input: Array[String]): Boolean = {
    input.length match {
      case 2 => true
      case _ => false
    }
  }

  def getGridPrint(grid: Grid): Unit = {

    val all = (0 to grid.size.y - 1)
      .map(e => makeRow(grid.size.x)).mkString("\n")

    println(all)
  }

  private def makeRow(length: Int): String = {
    val rowCarcass = (0 to  length-1).map(e => "|.").mkString("")
    rowCarcass + "|"
  }

}


class DisplayRover extends Display {

  def parseInputAndShowRover(input: Array[String], grid: Grid) = {
    val parsedCoords = parseInput(input)
    val gridSceleton = grid.generate().reverse
    val roverMap =
      gridSceleton
        .map(column => column
          .map(e =>
            e match {
              case parsedCoords.gridCoordinates => "|X"
              case _ => "|."
            }
          ))

    println(roverMap.map(e => e.mkString + "|\n").mkString)
  }


  def getInput: Array[String] = {
    println("Insert rover dimensions in format <Xloc Yloc Side>")
    val input = scala.io.StdIn.readLine()
    inputIsValid(input.split(" ")) match {
      case true => input.split(" ")
      case false => {
        println("The input has to be in format <Xloc Yloc Side>")
        getInput
      }
    }
  }

  def parseInput(input: Array[String]): RoverCoordinates = {
    val side = input(2) match {
      case "N" => North
      case "S" => South
      case "E" => East
      case "W" => West
    }
    RoverCoordinates(input(0).toInt, input(1).toInt, side)
  }


   def inputIsValid(input: Array[String]): Boolean = {

     input.length match {
       case 3 => true // TODO: more tests on the format of input
       case _ => false
     }

  }
}
