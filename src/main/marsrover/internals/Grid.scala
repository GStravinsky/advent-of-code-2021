package marsrover.internals

import main.marsrover.internals.domain.{Coordinates, Row}

case class Grid(size: Coordinates){
    def generate(coordinates: Coordinates = size): Vector[Row] = {
      (0 to coordinates.y-1) // y
        .map( column => (0 to coordinates.x-1).toVector//  0 -> (0,1,2)
          .map(row => Coordinates(column,row)) // (0,1,2) -> 0,0; 0,1; 0,2
        ).toVector
    }

}