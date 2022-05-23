package marsrover.internals

import main.marsrover.internals.domain.{Coordinates, Display}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class DisplayTest extends AnyFlatSpecLike  with Matchers {

  behavior of "Display"

  it should "create grid given the input" in {
    val coordTest = Coordinates(2,3)
    val gridTest = Grid(coordTest)

    val disp = new DisplayGrid

    disp.drawGrid(gridTest)
  }

}
