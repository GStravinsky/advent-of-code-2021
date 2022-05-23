package marsrover.internals

import marsrover.internals.domain._
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class RoverTest extends AnyFlatSpecLike  with Matchers {

  behavior of "RoverInstructionParser"

  it should "do stuff well" in {

    RoverInstructionParser.moveLeft(North) should be (West)
    RoverInstructionParser.moveRight(North) should be (East)
    RoverInstructionParser.moveAhead(RoverCoordinates(1,2, North)) should be (RoverCoordinates(1,3, North))

  }

}
