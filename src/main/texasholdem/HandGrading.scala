package texasholdem

trait Hand
case class RoyalFlush() extends Hand
case class StraightFlush() extends Hand
case class Flush() extends Hand
case class FourOfAKind() extends Hand // 4 cards of same symbols, symbols.length = 2
case class FullHouse() extends Hand // 2 symbols (3,2), symbols.length = 2
case class Straight() extends Hand // cards in row by symbol, symbols.length = 5
case class ThreeOfAKind() extends Hand // 3 cards of same symbol, symbols.length = 3  because FullHouse is 2
case class TwoPair() extends Hand // 2 pairs, symbols.length = 3
case class Pair() extends Hand // 1 pair, symbols.length = 4
case class HighestCards() extends Hand // has the highest card in the game, symbols.length = 5

object HandGrading {

  def chooseWinner(handOne: Seq[Card], handTwo: Seq[Card]): Hand = {
    // TODO: find the type of the hand
    // TODO: choose which one is better
  }

  def whatTypeOfHand(cards: Seq[Card]): Hand = {
    val suits = cards.map(_.getClass.getSimpleName)
    val symbols = cards.map(e => e.symbol)

    suits.length match {
      case 1 => sameSuitHandCheck(cards) // check for Royal Flush, StraightFLush, Flush
      case _ => symbols.length match {
        case 2 => twoSymbolsCheck(cards) //FourOfAKind, FullHouse
        case 3 => threeSymbolCheck(cards) //TheeOfAKind, TwoPair
        case 4 => Pair() //Pair
        case 5 => fiveSymbolCheck(cards) // Straight, HighestCard
      }
    }
  }

  // check for Royal Flush, StraightFLush, Flush
  private def sameSuitHandCheck(card: Seq[Card]): Hand = ???
  private def twoSymbolsCheck(card: Seq[Card]): Hand = ???
  private def threeSymbolCheck(card: Seq[Card]): Hand = ???
  private def fiveSymbolCheck(card: Seq[Card]): Hand = ???
}
