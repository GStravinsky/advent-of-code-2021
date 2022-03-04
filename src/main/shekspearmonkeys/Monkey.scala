package shekspearmonkeys
import scala.util.Random

case class Computer(guess:String){
  val TARGET = "METHINKS IT IS LIKE A WEASEL"
  def isCorrect = ???
  def score = ???
  def pickBestGuess = ???
}

case class Monkey(initialGuess: String){
  val KEYBOARD = Seq("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
    "T", "U", "V", "W", "X", "Y", "Z", " ")
  val numberOfGuesses = 100
  val probabilities = (1 to numberOfGuesses).map(_ => Random.nextInt(initialGuess.length))
  // Can we define that the length is 100?
  def copyAndMutate(guess: String): Seq[String] = {
    val copiedGuess = (1 to numberOfGuesses).map(_ => guess)
    copiedGuess.map(e => mutateGuess(e))
  }
  def mutateGuess(guess: String, probs: Seq[Int] = probabilities): String = {
    // nextInt is end exclusive
    val zipped = guess zip probs
    val mutated = zipped.map(e => if (e._2<5) KEYBOARD(Random.nextInt(KEYBOARD.length)) else e._1)
    mutated.mkString("")
  }
}