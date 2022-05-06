package shaney

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import shaney.Create.{ chooseNextWordBasedOnOneWord, getAllPossibleNextWordsWithProbabilities}

class CreateTest extends AnyFlatSpecLike  with Matchers {

  val testTrainingSet = Seq(
    Triple("The", "breeding", "of"),
    Triple("breeding", "of", "an"),
    Triple("of", "an", "animal"),
    Triple("of", "a", "Christ"),
    Triple("of", "an", "apple")
  )

  behavior of "Create"

  it should "choose the correct next word" in {
    chooseNextWordBasedOnOneWord("of", testTrainingSet.toArray) should (be ("an") or be ("a"))
  }
  it should "getAllPossibleNextWordsWithProbabilities" in {

    val expected = Map("an" -> 2.toDouble/3, "a"->1.toDouble/3)
    getAllPossibleNextWordsWithProbabilities("of", testTrainingSet.toArray) should contain theSameElementsAs expected
  }

}
