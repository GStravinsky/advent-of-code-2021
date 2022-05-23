package marsrover.internals

import marsrover.internals.domain.{Direction, East, GridCoordinates, Instructions, Move, North, RotateToLeft, RotateToRight, Rover, RoverCoordinates, South, West}

case class MarsRover(name: String, coordinates: RoverCoordinates, instructions: Array[Instructions]) extends Rover {

  val symbol = name.head

  def nextMove(): MarsRover = {
    instructions.head match {
      case RotateToLeft => {
        val newDirection = RoverInstructionParser.moveLeft(coordinates.direction)
        val newCoordinates = coordinates.copy(direction = newDirection)
        copy(coordinates = newCoordinates, instructions = instructions.tail)
      }
      case RotateToRight => {
        val newDirection = RoverInstructionParser.moveRight(coordinates.direction)
        val newCoordinates = coordinates.copy(direction = newDirection)
        copy(coordinates = newCoordinates, instructions = instructions.tail)
      }
      case Move => {
        val newCoordinates = RoverInstructionParser.moveAhead(coordinates)
        copy(coordinates = newCoordinates, instructions = instructions.tail)
      }
    }
  }


}

case object RoverInstructionParser {

//  val t = MarsRover("X", RoverCoordinates(1,2, North), instructions)
//
//  Iterator.iterate(t)(_.nextMove).takeWhile(_.instructions.length>0)

  def moveLeft(currentDirection: Direction): Direction = {
    currentDirection match {
      case North => West
      case East => North
      case South => East
      case West => South
    }
  }

  def moveRight(currentDirection: Direction): Direction = {
    currentDirection match {
      case North => East
      case East => South
      case South => West
      case West => North
    }
  }

  def moveAhead(currentCoordinates: RoverCoordinates): RoverCoordinates = {
    currentCoordinates.direction match {
      case East => currentCoordinates.copy(x = currentCoordinates.x + 1)
      case West => currentCoordinates.copy(x = currentCoordinates.x - 1) // TODO: border cases, needs notion of a grid
      case South => currentCoordinates.copy(y = currentCoordinates.y - 1)
      case North => currentCoordinates.copy(y = currentCoordinates.y + 1)

    }
  }
}

