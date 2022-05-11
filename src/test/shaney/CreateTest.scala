package shaney

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import shaney.Create.{chooseNextWord, getAllPossibleNextWordsWithProbabilities, getCDF}

class CreateTest extends AnyFlatSpecLike  with Matchers {

  val testTrainingSet = Seq(
    Triple("The", "breeding", "of"),
    Triple("breeding", "of", "an"),
    Triple("of", "an", "animal"),
    Triple("of", "a", "Christ"),
    Triple("of", "an", "apple")
  )

  behavior of "Create"

  it should "create CDF" in {
    val input = Seq(
      Distribution("a", 0.1),
      Distribution("b", 0.2),
      Distribution("c", 0.5),
      Distribution("d", 0.2)
    )
    val expected = Seq(
      Distribution("a", 0.1),
      Distribution("b", 0.3),
      Distribution("c", 0.8),
      Distribution("d", 1.0)
    )

    getCDF(input) shouldBe expected
  }

  it should "choose the correct next word" in {
    chooseNextWord("of", testTrainingSet.toArray) should (be ("an") or be ("a"))
  }
  it should "getAllPossibleNextWordsWithProbabilities" in {

    val expected = Map("an" -> 2.toDouble/3, "a"->1.toDouble/3)
    getAllPossibleNextWordsWithProbabilities("of", testTrainingSet.toArray) should contain theSameElementsAs expected
  }

}
