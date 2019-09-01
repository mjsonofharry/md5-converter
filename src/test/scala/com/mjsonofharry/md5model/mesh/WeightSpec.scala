package com.mjsonofharry.md5model.mesh

import org.scalatest._
import atto._, Atto._
import cats.implicits._
import atto.ParseResult.Done

class WeightSpec extends FlatSpec with Matchers with GivenWhenThen with Inside {
  val WEIGHT = "	weight 20 38 0.388059 1.915334 -1.046617 0.109413"

  "the weight parser" should "successfully parse a weight" in {
    Given("a weight")
    val weight = WEIGHT

    When("the weight is parsed")
    val result = Weight.parser.parseOnly(weight)

    Then("a weight object should be constructed")
    result should matchPattern { case Done(_: String, _: Weight) => }

    inside(result) {
      case Done(_: String, weight: Weight) =>
        inside(weight) {
          case Weight(
              index: Int,
              jointIndex: Int,
              bias: Double,
              position: List[Double]
              ) => {
            index shouldBe 20
            jointIndex shouldBe 38
            bias shouldBe 0.388059
            position should contain theSameElementsInOrderAs List(
              1.915334,
              -1.046617,
              0.109413
            )
          }
        }
    }
  }
}
