object Day2 {
  def main(args: Array[String]): Unit = {
    val bufferedSource = io.Source.fromFile("input_big.txt")
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close


    val sum_forward = lines.filter(
      x => x.contains("forward")
    ).map(
      e => e.split(" ")(1).toInt
    ).foldLeft(0)(_+_)

    // if up, add minus sign
    val sum_depth = lines.filter(
      x => ! x.contains("forward")
    ).map(
        e => if (e.split(" ")(0).contains("up"))
            -e.split(" ")(1).toInt
            else
            e.split(" ")(1).toInt
      ).foldLeft(0)(_+_)

    println(sum_depth * sum_forward)

  }

}
