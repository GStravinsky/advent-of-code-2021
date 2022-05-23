package marsrover

import marsrover.internals.{DisplayGrid, Grid}

object MarsRover {

  def main(args: Array[String]): Unit = {

    val displayGrid = new DisplayGrid
    val input = displayGrid.getInput
    displayGrid.parseInputAndDrawGrid(input)
  }
}
