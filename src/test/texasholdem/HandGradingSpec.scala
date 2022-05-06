package texasholdem

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import texasholdem.HandGrading._

class HandGradingSpec extends AnyFlatSpecLike  with Matchers {

  val royalFLushTest = Seq(
      Diamonds(10), Diamonds(14), Diamonds(13), Diamonds(12), Diamonds(11)
    )
  val highestCardTest = Seq(
    Diamonds(14), Spades(2), Clubs(5), Clubs(10), Spades(8)
  )
  val fullHouseTest = Seq(
    Diamonds(10), Spades(10), Clubs(8), Clubs(8), Clubs(8)
  )
  val pairTest = Seq(
    Diamonds(11), Hearts(11), Clubs(8), Clubs(7), Spades(9)
  )
  val threeOfAKindTes = Seq(
    Diamonds(10), Spades(10), Hearts(10), Spades(8), Diamonds(12)
  )

  val straightFlushTest = Seq(Diamonds(4), Diamonds(5), Diamonds(6), Diamonds(7), Diamonds(8))


  it should "deduce hand types correctly" in {
    whatTypeOfHand(royalFLushTest) shouldBe RoyalFlush()
    whatTypeOfHand(highestCardTest) shouldBe HighestCard()
    whatTypeOfHand(fullHouseTest) shouldBe FullHouse()
    whatTypeOfHand(pairTest) shouldBe Pair()
    whatTypeOfHand(threeOfAKindTes) shouldBe ThreeOfAKind()
    whatTypeOfHand(straightFlushTest) shouldBe StraightFlush()
  }

  it should "know which hand is better when hands are of different types" in {
    chooseWinner(royalFLushTest, highestCardTest) shouldBe (royalFLushTest, RoyalFlush())
    chooseWinner(pairTest, fullHouseTest) shouldBe (fullHouseTest, FullHouse())
  }

  it should "decide the best hand from kicker in two one draw hand cases" in {
    val straightFlushWorse = Seq(Diamonds(2), Diamonds(3), Diamonds(4), Diamonds(5), Diamonds(6))
    chooseWinner(straightFlushWorse, straightFlushTest) shouldBe (straightFlushTest, StraightFlush())
  }

  it should "decide the best hand from the second draw in two two draw cases"

  it should "decide the best hand from a kicker in two two draw cases with equal second draw"

  it should "decide the best hand from 5 card kicker in Highest card case"
}
