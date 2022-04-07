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

  it should "deduce hand types correctly" in {
    whatTypeOfHand(royalFLushTest) shouldBe RoyalFlush()
    whatTypeOfHand(highestCardTest) shouldBe HighestCard()
    whatTypeOfHand(fullHouseTest) shouldBe FullHouse()
    whatTypeOfHand(pairTest) shouldBe Pair()
    whatTypeOfHand(threeOfAKindTes) shouldBe ThreeOfAKind()
  }

}
