package texasholdem

trait Hand {
  val score: Int
}

case class RoyalFlush() extends Hand {
  val score = 10
} //A-K-Q-J-10 same suit, check on symbols
case class StraightFlush() extends Hand {
  val score = 9
} // same suit, check dif between symbols is always 1 after ordering
case class Flush() extends Hand {
  val score = 8
} // else if not the two above
case class FourOfAKind() extends Hand {
  val score = 7
} // 4 cards of same symbols, symbols.length = 2
case class FullHouse() extends Hand {
  val score = 6
} // 2 symbols (3,2), symbols.length = 2
case class Straight() extends Hand {
  val score = 5
} // cards in row by symbol, symbols.length = 5
case class ThreeOfAKind() extends Hand {
  val score = 4
} // 3 cards of same symbol, symbols.length = 3  because FullHouse is 2
case class TwoPair() extends Hand {
  val score = 3
}// 2 pairs, symbols.length = 3
case class Pair() extends Hand {
  val score = 2
} // 1 pair, symbols.length = 4
case class HighestCard() extends Hand  {
  val score = 1
}// has the highest card in the game, symbols.length = 5

object HandGrading {

  def chooseWinner(handOne: Seq[Card], handTwo: Seq[Card]): (Seq[Card], Hand) = {
    val typeHandOne = whatTypeOfHand(handOne)
    println(s"First hand is of type $typeHandOne")
    val typeHandTwo = whatTypeOfHand(handTwo)
    println(s"Second hand is of type $typeHandTwo")

    typeHandOne.score > typeHandTwo.score match {
      case true => (handOne, typeHandOne)
      case false => typeHandOne.score == typeHandTwo.score match {
        case true => sameTypeHandWinner(handOne, handTwo, typeHandOne) //some logic for equality kickers
        case false => (handTwo, typeHandTwo)
      }
    }
  }

 private def sameTypeHandWinner(handOne: Seq[Card], handTwo: Seq[Card], typeHand: Hand): (Seq[Card], Hand) = {

   typeHand match {
     case StraightFlush() | Flush() | Straight() => (highestSumOfCards(handOne, handTwo).get, typeHand)
     case FourOfAKind() | ThreeOfAKind() | Pair() => (oneDraw(handOne, handTwo), typeHand)
     case TwoPair() | FullHouse() => (twoDraws(handOne, handTwo), typeHand)
     case HighestCard() => (decideFromKicker(handOne, Seq.empty, handTwo, Seq.empty), typeHand)

   }
  }

  private def oneDraw(handOne: Seq[Card], handTwo: Seq[Card]): Seq[Card] = {

    val mainDrawOne = getMainDraw(handOne)
    val mainDrawTwo = getMainDraw(handTwo)

    val mainDrawOneSum = mainDrawOne.map(_.symbol).foldLeft(0)(_+_)
    val mainDrawTwoSum = mainDrawTwo.map(_.symbol).foldLeft(0)(_+_)

    mainDrawOneSum > mainDrawTwoSum match {
      case true => handOne
      case false => mainDrawOneSum == mainDrawTwoSum match {
        case true => decideFromKicker(handOne, mainDrawOne, handTwo, mainDrawTwo)
        case false => handTwo
      }
    }
  }


  private def twoDraws(handOne: Seq[Card], handTwo: Seq[Card]): Seq[Card] = {

    val mainDrawOne = getMainDraw(handOne)
    val mainDrawTwo = getMainDraw(handTwo)

    val mainDrawOneSum = mainDrawOne.map(_.symbol).foldLeft(0)(_+_)
    val mainDrawTwoSum = mainDrawTwo.map(_.symbol).foldLeft(0)(_+_)

    mainDrawOneSum > mainDrawTwoSum match {
      case true => handOne
      case false => mainDrawOneSum == mainDrawTwoSum match {
        case true => {
          val secondDrawOne = handOne diff mainDrawOne
          val seconfDrawTwo = handTwo diff mainDrawTwo
          oneDraw(secondDrawOne, seconfDrawTwo)
        }
        case false => handTwo
      }
    }
  }

  private def getMainDraw(cards: Seq[Card]):  Seq[Card] = {
    val freqs: Map[Int,Int] = cards.groupBy(identity).map(e=> e._1.symbol -> e._2.length)

    // finds first most common element
    val mostCommon = freqs.maxBy(_._2)

    // the following two lines are for the TwoPair case only
    val allMostCommon = freqs.filter(e => e._2 == mostCommon._2)
    val mainDrawCardSymbol = allMostCommon.maxBy(_._1)

    cards.filter( e=> e.symbol == mainDrawCardSymbol._1)
  }

  private def decideFromKicker(handOne: Seq[Card], drawOne: Seq[Card], handTwo: Seq[Card], drawTwo: Seq[Card]): Seq[Card] = {
    val kickerOne = handOne diff drawOne
    val kickerTwo = handTwo diff drawTwo

    val sortOne = kickerOne.sortBy(_.symbol)(Ordering.Int.reverse)
    val sortTwo = kickerTwo.sortBy(_.symbol)(Ordering.Int.reverse)

    val difsInVal = (sortOne zip sortTwo).map{ case (one, two) => one.symbol - two.symbol}

    val firstCarSymbolDifference = difsInVal.find(e => e != 0)

    firstCarSymbolDifference.get > 0 match {
      case true => handOne
      case false => handTwo
    }
  }


  private def highestSumOfCards(handOne: Seq[Card], handTwo: Seq[Card]): Option[Seq[Card]] = {
    val sumOne = handOne.map(_.symbol).foldLeft(0)(_+_)
    val sumTwo = handTwo.map(_.symbol).foldLeft(0)(_+_)

    sumOne > sumTwo match {
      case true => Some(handOne)
      case false => sumOne == sumTwo match {
        case true => None
        case false => Some(handTwo)
      }
    }
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
      case _ => symbolsOrd.sliding(2).toList.map{case Seq(a,b) => a-b}  match {
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
