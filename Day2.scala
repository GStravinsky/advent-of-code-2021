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

    println(s"The solution number one is: ${sum_depth * sum_forward}")

// Part 2: the order matters

    // function to get depth

    var depth = 0
    var aim = 0

    for (i <- lines) {

      val sep_elem = i.split(" ")
      sep_elem(0) match {
        case "forward" => depth += sep_elem(1).toInt * aim
        case "up" => aim -= sep_elem(1).toInt
        case "down" => aim += sep_elem(1).toInt
      }
    }

    println(s"The solution number two is: ${depth * sum_forward}")

  }

}
