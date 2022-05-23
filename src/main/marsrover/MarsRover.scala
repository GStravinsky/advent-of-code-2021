package marsrover

import marsrover.internals.{DisplayGrid, DisplayRover, Grid}

object MarsRover {

  def main(args: Array[String]): Unit = {

    val displayGrid = new DisplayGrid
    val inputGrid = displayGrid.getInput
    val gridSceleton = displayGrid.parseInputAndGetrid(inputGrid)
    displayGrid.getGridPrint(gridSceleton)

    val displayRover = new DisplayRover
    val inputRover = displayRover.getInput
    displayRover.parseInputAndShowRover(inputRover, gridSceleton )

  }
}
