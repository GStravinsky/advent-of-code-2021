package texasholdem

object Game {

  def main(args: Array[String]): Unit = {
    val dealer = Dealer()
    val currentDeck = dealer.makeDeck()
    val playerOne = Player("Gabby")
    val playerTwo = Player("Bob")

    println(s"Head of deck: ${currentDeck.cards.head}")
    println(s"There are ${currentDeck.cards.length} cards in the deck")
    println(Console.BLUE + "Hole")
    val (deckHole, dealtPlayers) = dealer.holeCards(currentDeck, Seq(playerOne,playerTwo))
    dealer.discardCard(deckHole,1)
    println(Console.YELLOW + s"Player cards: ${dealtPlayers.map(e => e.name -> e.cards).toMap}")
    println(Console.WHITE + s"There are ${deckHole.cards.length} cards in the deck")
    println(Console.BLUE + "Flop")
    val (deckFlop, table3) = dealer.flop(deckHole)
    dealer.discardCard(deckFlop,1)
    println(Console.YELLOW + s"Table: ${table3.cards.get}")
    println(Console.WHITE + s"There are ${deckFlop.cards.length} cards in the deck")
    println(Console.BLUE + "Turn")
    val (deckTurn, table4) = dealer.turn(deckFlop, table3)
    dealer.discardCard(deckTurn,1)
    println(Console.YELLOW + s"Table: ${table4.cards.get}")
    println(Console.WHITE  + s"There are ${deckTurn.cards.length} cards in the deck")
    println(Console.BLUE  + "River")
    val (deckRiver, table5) = dealer.river(deckTurn, table4)
    println(Console.YELLOW + s"Table: ${table5.cards.get}")
    println(Console.WHITE  + s"There are ${deckRiver.cards.length} cards in the deck")
    println(Console.RED + "Let the game begin.")
  }
}
