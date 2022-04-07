package texasholdem

trait Hand
case class RoyalFlush() extends Hand //A-K-Q-J-10 same suit, check on symbols
case class StraightFlush() extends Hand // same suit, check dif between symbols is always 1 after ordering
case class Flush() extends Hand // else if not the two above
case class FourOfAKind() extends Hand // 4 cards of same symbols, symbols.length = 2
case class FullHouse() extends Hand // 2 symbols (3,2), symbols.length = 2
case class Straight() extends Hand // cards in row by symbol, symbols.length = 5
case class ThreeOfAKind() extends Hand // 3 cards of same symbol, symbols.length = 3  because FullHouse is 2
case class TwoPair() extends Hand // 2 pairs, symbols.length = 3
case class Pair() extends Hand // 1 pair, symbols.length = 4
case class HighestCard() extends Hand // has the highest card in the game, symbols.length = 5

object HandGrading {

  def chooseWinner(handOne: Seq[Card], handTwo: Seq[Card]): Unit = {
    // TODO: find the type of the hand
    // TODO: choose which one is better
  }

  def whatTypeOfHand(cards: Seq[Card]): Hand = {
    val suits = cards.map(_.getClass.getSimpleName)
    val symbols = cards.map(e => e.symbol)

    suits.distinct.length match {
      case 1 => sameSuitHandCheck(symbols) // check for Royal Flush, StraightFLush, Flush
      case _ => symbols.distinct.length match {
        case 2 => twoSymbolsCheck(symbols) //FourOfAKind, FullHouse
        case 3 => threeSymbolCheck(symbols) //TheeOfAKind, TwoPair
        case 4 => Pair() //Pair
        case 5 => fiveSymbolCheck(symbols) // Straight, HighestCard
      }
    }
  }

  // check for Royal Flush, StraightFLush, Flush
  private def sameSuitHandCheck(symbols: Seq[Int]): Hand = {
    val symbolsOrd = symbols.sorted(Ordering.Int.reverse)

    symbolsOrd match {
      case Seq(14,13,12,11,10) => RoyalFlush()
      case _ => symbolsOrd.sliding(2).map{case List(a,b) => a-b} match {
        case List(1,1,1,1) => StraightFlush()
        case _ => Flush()
      }
    }
  }

  // check for FourOfAKind or FullHouse
  private def twoSymbolsCheck(symbols: Seq[Int]): Hand = {
    val existingSymbols = symbols.distinct
    val frequencies = existingSymbols.map(e => e -> symbols.count(s => s ==e )).toMap

    frequencies.values.max match {
      case 4 => FourOfAKind()
      case 3 => FullHouse()
    }
  }

  // check TheeOfAKind, TwoPair
  private def threeSymbolCheck(symbols: Seq[Int]): Hand = {
    val existingSymbols = symbols.distinct
    val frequencies = existingSymbols.map(e => e -> symbols.count(s => s ==e )).toMap

    frequencies.values.max match {
      case 3 => ThreeOfAKind()
      case 2 => TwoPair()
    }
  }

  // Straight, HighestCard
  private def fiveSymbolCheck(symbols: Seq[Int]): Hand = {
    val symbolsOrd = symbols.sorted(Ordering.Int.reverse)

    symbolsOrd.sliding(2).map{case List(a,b) => a-b} match {
      case List(1,1,1,1) => Straight()
      case _ => HighestCard()
    }
  }
}
