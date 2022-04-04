object Options {

  def addOptions(optionA: Option[Int], optionB: Option[Int], optionC: Option[Int]): Option[Int] = {
   val t = for {
      first <- optionA
      seconf <- optionB
      third <- optionC
    } yield first + seconf + third
    t
  }

  def addOptionsMap(optionA: Option[Int], optionB: Option[Int], optionC: Option[Int]): Option[Int] = {
    optionA.flatMap(e => optionB.map(b => optionC.map(c => e+b+c)))
  }
  def main(args: Array[String]): Unit = {
    println(addOptions(None, Some(10). Some(2)))
    println(addOptionsMap(Some(1), Some(10), Some(2)))
  }


}