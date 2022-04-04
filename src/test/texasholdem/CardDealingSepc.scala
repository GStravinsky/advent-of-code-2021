package texasholdem

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class TexasHoldEm extends AnyFlatSpecLike  with Matchers {

  val dealer = Dealer()
  val currentDeck = dealer.makeDeck()
  val playerOne = Player("p1")
  val playerTwo = Player("p2")
  val table = Table()

  val (deck48, dealtPlayers) = dealer.holeCards(currentDeck, Seq(playerOne,playerTwo))
  val (deck44, table3) = dealer.flop(deck48)
  val (deck42, table4) = dealer.turn(deck44, table3)
  val (deck40, table5) = dealer.river(deck42, table4)


  it should "make a full schuffled deck" in {
    currentDeck.cards.length shouldBe 52
  }

  it should "deal hole cards" in {
    deck48.cards.length shouldBe 48
    dealtPlayers.map(_.cards.get.length) shouldBe Seq(2,2)
  }

  it should "deal flop" in {
    deck44.cards.length shouldBe 44
    table3.cards.get.length shouldBe 3
  }

  it should "deal turn" in {
    deck42.cards.length shouldBe 42
    table4.cards.getOrElse(Seq.empty).length shouldBe 4
  }

  it should "deal river" in {
    deck40.cards.length shouldBe 40
    table5.cards.getOrElse(Seq.empty).length shouldBe 5
  }
}
