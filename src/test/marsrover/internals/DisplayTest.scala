package marsrover.internals

import marsrover.internals.domain.{GridCoordinates, Display}
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
    val input = Array("1","2","N")

    disp.parseInputAndShowRover(input, gridTest)
  }

}
