package marsrover.internals

import main.marsrover.internals.domain.{Coordinates, Display}

class DisplayGrid extends Display{


  def parseInputAndDrawGrid(input: Array[String]) = {
    val parsedCoords = parseInput(input)
    val grid = Grid(parsedCoords)
    drawGrid(grid)
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

  override def parseInput(input: Array[String]): Coordinates = {
    Coordinates(input.head.toInt, input.tail.head.toInt )
  }

  def inputIsValid(input: Array[String]): Boolean = {
    input.length match {
      case 2 => true
      case _ => false
    }
  }

  def drawGrid(grid: Grid): Unit = {

    val all = (0 to grid.size.y - 1 )
      .map(e => makeRow(grid.size.x)).mkString("\n")
    println(all)
  }

  private def makeRow(length: Int): String = {
    val rowCarcass = (0 to  length-1).map(e => "|.").mkString("")
    rowCarcass + "|"
  }

}


