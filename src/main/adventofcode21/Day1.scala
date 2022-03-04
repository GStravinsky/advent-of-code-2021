object Day1 {
  def main(args: Array[String]): Unit = {
    val bufferedSource = io.Source.fromFile("input_big.txt")
    val lines = (for (line <- bufferedSource.getLines()) yield line.toInt).toList
    bufferedSource.close

    var flat_count = 0

    for (i <- 0 to lines.length-2) {
      val difference = lines(i + 1) - lines(i)
      if (difference > 0) flat_count += 1
    }

    println(s"Counting flat map gives us ${flat_count} differences")

    var three_step_count = 0
    for (i <- 0 to lines.length-3) {
      val sum1 = lines.slice(i, i+3).foldLeft(0)(_+_)
      val sum2 = lines.slice(i+1, i+4).foldLeft(0)(_+_)
      if (sum2-sum1 > 0) three_step_count += 1
    }

    println(s"Counting 3-step map gives us ${three_step_count} differences")

  }

}
