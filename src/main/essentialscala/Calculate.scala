package essentialscala

object Calculate {
  def main(args: Array[String]): Unit = {
    println(divide(1,0))
    println(divide(2,3))
  }

  def divide(numerator:Int, denominator:Int): Option[Double] = {
    denominator match {
      case 0 => None
      case _ => Some(numerator.toFloat / denominator)
    }
  }

  def divideOptions(num: Option[Int], denom: Option[Int]): Option[Double] = {
    for {
      a <- num
      b <- denom
      c <- divide(a,b)
    } yield c
  }
}