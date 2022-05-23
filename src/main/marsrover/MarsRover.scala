package marsrover

import marsrover.internals.{DisplayGrid, DisplayRover, DisplayRoverInstructions, Grid, MarsRover}

object Explore {

  def main(args: Array[String]): Unit = {

    val displayGrid = new DisplayGrid
    val inputGrid = displayGrid.getInput
    val gridSceleton = displayGrid.parseInputAndGetrid(inputGrid)
    displayGrid.getGridPrint(gridSceleton)

    val displayRover = new DisplayRover
    val inputRover = displayRover.getInput
    val roverCoordinates = displayRover.parseInput(inputRover)

    val displayRoverInstructions = new DisplayRoverInstructions
    val inputRoverInstructions = displayRoverInstructions.getInput
    val instructions = displayRoverInstructions.parseInput(inputRoverInstructions)

    val marsRover = MarsRover("X", roverCoordinates, instructions)

    val states = Iterator.iterate(marsRover)(_.nextMove).take(instructions.length+1)//.takeWhile(_.instructions.length!=0)

    for (state <- states) {
      displayRover.showRover(state.coordinates,gridSceleton)
    }

  }
}
