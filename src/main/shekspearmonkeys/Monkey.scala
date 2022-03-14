package shekspearmonkeys
import scala.annotation.tailrec
import scala.util.Random

trait SimulationStatus
case object Correct extends SimulationStatus
case object InProgress extends SimulationStatus

case class ShakespeareSimulation(status: SimulationStatus, monkey: Monkey, computer: Computer = Computer()) {

  def nextState(): ShakespeareSimulation = {
    status match {
      case Correct => null
      case InProgress =>
        val monkeyGuess = monkey.copyAndMutate()
        val (bestString, score)  = computer.pickBestGuess(monkeyGuess)
        println(f"Best GUESS: $bestString")

        val newStatus = score match {
          case 28 => Correct
          case _ => InProgress
        }
        copy(status = newStatus, monkey = Monkey(bestString))
    }
  }
}

case class Computer() {
  val TARGET = "METHINKS IT IS LIKE A WEASEL"

  private def score(givenGuess: String): Int = {
    val isCharCorrect =  (0 to givenGuess.length-1).map(
      e => if (givenGuess(e)== TARGET(e)) 1 else 0
    )
    isCharCorrect.foldLeft(0)(_+_)
  }

  def pickBestGuess(givenGuesses: Seq[String]): (String, Int) = {
    val scores = givenGuesses.map(e => score(e))
    val bestScoreIndex = scores.indexOf(scores.max)
    (givenGuesses(bestScoreIndex), scores.max)
  }
}

case class Monkey(guess: String){

  val KEYBOARD = Seq("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
    "T", "U", "V", "W", "X", "Y", "Z", " ")
  val numberOfGuesses = 100
  // this needs to be done as a def so it would be dynamic
  private def generateProbabilities =
    (1 to numberOfGuesses).map(_ => Random.nextInt(numberOfGuesses))

  // Can we define that the length is 100?
  def copyAndMutate(givenGuess: String = guess): Seq[String] = {
    val copiedGuess = (1 to numberOfGuesses).map(_ => givenGuess)
    copiedGuess.map(e => mutateGuess(e))
  }

  private [shekspearmonkeys] def mutateGuess(givenGuess: String = guess, probs: Seq[Int] = generateProbabilities): String = {
    // nextInt is end exclusive
    val zipped = givenGuess zip probs
    val mutated = zipped.map(e => if (e._2<5) KEYBOARD(Random.nextInt(KEYBOARD.length)) else e._1)
    mutated.mkString("")
  }
}

object ShakespeareSimulation{

  @tailrec
  private def nextValidGuess(): String = {
    println("Enter 28 symbol input")
    val initialGuess = scala.io.StdIn.readLine().toUpperCase
    initialGuess.length match {
      case 28 => initialGuess
      case _ =>
        println("The input has to be 28 symbols long")
        nextValidGuess()
    }
  }
  def main(args: Array[String]): Unit = {
   // "SKTOFPRHTISHROPWRNT O RUFNT "
    val initialGuess = nextValidGuess()

    val simulation = ShakespeareSimulation(InProgress, Monkey(initialGuess))
    val states = Iterator.iterate(simulation)(_.nextState()).takeWhile(_ != null)
    println(f"The correct state reached in ${states.length} steps")
    }

}