package marsrover.internals

import marsrover.internals.domain.GridCoordinates
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class GridTest extends AnyFlatSpecLike  with Matchers {

  behavior of "Grid"

  it should "create grid given the input" in {
    val coordTest = GridCoordinates(2,3)

    val grid = Grid(coordTest).generate()

    val expectedRowLength = grid.map(r=> r.length).distinct.head


    grid.length should be (3)
    expectedRowLength should be (2)
  }

}
