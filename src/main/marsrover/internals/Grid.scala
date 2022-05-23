package marsrover.internals

import marsrover.internals.domain.{GridCoordinates, Row}

case class Grid(size: GridCoordinates){
    def generate(coordinates: GridCoordinates = size): Vector[Row] = {
      (0 to coordinates.y-1) // y
        .map( column => (0 to coordinates.x-1).toVector//  0 -> (0,1,2)
          .map(row => GridCoordinates(row,column)) // (0,1,2) -> 0,0; 0,1; 0,2
        ).toVector
    }

}