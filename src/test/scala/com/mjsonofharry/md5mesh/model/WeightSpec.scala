package com.mjsonofharry.md5mesh.model

import org.scalatest._
import atto.ParseResult.Done

class WeightSpec extends FlatSpec with Matchers with GivenWhenThen with Inside {
  val weight = "	weight 20 38 0.388059 1.915334 -1.046617 0.109413"

  "the weight parser" should "successfully parse a weight" in {
    Given("a weight")
    val w = weight

    When("the weight is parsed")
    val result = Weight.parse(w)

    Then("a weight object should be constructed")
    result should matchPattern { case Done(_: String, _: Weight) => }

    inside(result) {
      case Done(_: String, weight: Weight) =>
        inside(weight) {
          case Weight(
              index: Int,
              jointIndex: Int,
              bias: Double,
              position: Vector3
              ) => {
            index shouldBe 20
            jointIndex shouldBe 38
            bias shouldBe 0.388059
            position.x shouldBe 1.915334
            position.y shouldBe -1.046617
            position.z shouldBe 0.109413
          }
        }
    }
  }
}
