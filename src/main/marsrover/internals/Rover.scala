package marsrover.internals

import marsrover.internals.domain.{GridCoordinates, Rover, RoverCoordinates}

case class MarsRover(name: String, coordinates: RoverCoordinates) extends Rover {

  val symbol = name.head


}


