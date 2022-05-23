package marsrover.internals

import marsrover.internals.domain.{Display, GridCoordinates, North, RoverCoordinates}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DisplayTest extends AnyFlatSpecLike  with Matchers {

  behavior of "DisplayGrid"

  it should "create grid given the input" in {
    val coordTest = GridCoordinates(2,3)
    val gridTest = Grid(coordTest)

    val disp = new DisplayGrid

    disp.getGridPrint(gridTest)
  }

  behavior of "DisplayRover"

  it should "create grid given the input" in {
    val coordTest = GridCoordinates(3,6)
    val gridTest = Grid(coordTest)

    val disp = new DisplayRover
    val input = RoverCoordinates(1,2,North)

    disp.showRover(input, gridTest)
  }

}
