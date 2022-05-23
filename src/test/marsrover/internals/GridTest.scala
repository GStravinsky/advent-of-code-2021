package marsrover.internals

import main.marsrover.internals.domain.Coordinates
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class GridTest extends AnyFlatSpecLike  with Matchers {

  behavior of "Grid"

  it should "create grid given the input" in {
    val coordTest = Coordinates(2,3)

    val grid = Grid(coordTest).generate()

    val expectedRowLength = grid.map(r=> r.length).distinct.head


    grid.length should be (3)
    expectedRowLength should be (2)
  }

}
