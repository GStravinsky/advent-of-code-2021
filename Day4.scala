
object Day4 {

  def main(args: Array[String]): Unit = {
    val (draw, tables) = loadData("input_big.txt")
    println(get_winner_board(draw, tables))
  }

  def loadData(filePath: String): (List[String], Map[Int, List[Set[String]]]) = {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close

    val bingo_draw = lines(0).split(",").toList

    val bingo_matrices = lines.slice(2, lines.length).sliding(5,6).toList.map{
      e => e.map{
        nums_str => nums_str.split("\\s+").toList.filter(s => s.matches("[0-9]+"))
      }
    }

    val BingoWithTranspose = bingo_matrices.map{
      e => e ::: e.transpose
    }

    val map_bingo_matrices=BingoWithTranspose.indices.map(x => x -> BingoWithTranspose(x)).toMap.map{
      case (k,v) => (k, v.map{
        e => e.toSet
      })
    }

    (bingo_draw, map_bingo_matrices)
  }

  def get_winner_board(
                        draws: List[String], BingoTables: Map[Int, List[Set[String]]],
                        last_number_index: Int = 5, Winners: Map[Int, List[Boolean]] = Map()
                      ): Any = {

    if (!Winners.isEmpty) {
      val winningTable = Winners.keys.head

      val SumOfUnmarked = (BingoTables(winningTable).flatten.toSet -- draws.slice(0, last_number_index-1).toSet).map{
        e => e.toInt
      }.foldLeft(0)(_+_)

      return (SumOfUnmarked * draws(last_number_index-2).toInt)
    }
    // min slice is from 5
    val current_draw = draws.slice(0, last_number_index).toSet
    val MatchingAxes = BingoTables.map{
      case (k,v) => (k, v.map{
        e => e.subsetOf(current_draw)
      })
    }.filter(x => x._2.contains(true))

    get_winner_board(draws, BingoTables, last_number_index+1, MatchingAxes)
  }


}