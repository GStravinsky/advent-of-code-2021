package texasholdem

import scala.util.Random

trait Card
case class Spades(symbol: Int) extends Card
case class Diamonds(symbol: Int) extends Card
case class Clubs(symbol: Int) extends Card
case class Hearts(symbol: Int) extends Card

case class Deck(cards: Seq[Card])

case class Player(name: String, cards: Option[Seq[Card]] = None)

case class Table(cards: Option[Seq[Card]] = None)

case class Dealer(){

  def makeDeck(): Deck = {
    val SYMBOLS = Range(2,15)
    val sortedDeck =
      SYMBOLS.map(e=> Spades(e)) ++ SYMBOLS.map(e=>Diamonds(e)) ++ SYMBOLS.map(e => Clubs(e)) ++ SYMBOLS.map(e=> Hearts(e))

    Deck(Random.shuffle(sortedDeck))
  }

  private def holeCardsOnePlayer(deck: Deck, player: Player):(Deck, Player) = {
    val cardfullPlayer = player.copy(cards = Some(deck.cards.take(2)))
    (discardCard(deck, 2), cardfullPlayer)
  }

  def holeCards(deck: Deck, players: Seq[Player]): (Deck, Seq[Player]) = {

    players.map(_.cards).contains(None) match {
      case false => (deck, players)
      case true => {
        val nextPlayer = players.filter(_.cards == None).head
        val (deckNew, playerWithCards) = holeCardsOnePlayer(deck, nextPlayer)
        val newPlayerStage = players.filterNot(e => e.name == nextPlayer.name) :+ playerWithCards
        holeCards(deckNew, newPlayerStage)
      }
    }
  }

  def flop(deck: Deck): (Deck, Table) = {
    val deckMinusOne = discardCard(deck,1)
    (discardCard(deckMinusOne,3), Table(cards = Some(deckMinusOne.cards.take(3))))
  }

  def turn(deck: Deck, table: Table): (Deck, Table) = {
    val deckMinusOne = discardCard(deck,1)

    table.cards.getOrElse(Seq.empty).length match {
      case 3 => (discardCard(deckMinusOne,1), Table(cards = Some(table.cards.get ++ deckMinusOne.cards.take(1))))
      case _ => throw new Exception(s"Turn must be called when table has 3 hard, not ${table.cards.get.length}")
    }
  }

  def river(deck: Deck, table: Table): (Deck, Table) = {
    val deckMinusOne = discardCard(deck,1)

    table.cards.getOrElse(Seq.empty).length match {
      case 4 => (discardCard(deckMinusOne,1), Table(cards = Some(table.cards.get ++ deckMinusOne.cards.take(1))))
      case _ => throw new Exception(s"River must be called when table has 4 hard, not ${table.cards.get.length}")
    }
  }

  def discardCard(deck: Deck, number:Int): Deck = Deck(deck.cards.drop(number))
}
