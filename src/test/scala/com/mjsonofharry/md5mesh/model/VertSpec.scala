package com.mjsonofharry.md5mesh.model

import org.scalatest._
import atto.ParseResult.Done

class VertSpec extends FlatSpec with Matchers with GivenWhenThen with Inside {
  val vert = "	vert 18 0.004956 0.823437 38 2"

  "the vert parser" should "successfully parse a vert" in {
    Given("a vert")
    val v = vert

    When("the vert is parsed")
    val result = Vert.parse(v)

    Then("a vert object should be constructed")
    result should matchPattern { case Done(_: String, _: Vert) => }

    inside(result) {
      case Done(_: String, vert: Vert) =>
        inside(vert) {
          case Vert(
              index: Int,
              u: Double,
              v: Double,
              weightStart: Int,
              weightCount: Int
              ) => {
            index shouldBe 18
            u shouldBe 0.004956
            v shouldBe 0.823437
            weightStart shouldBe 38
            weightCount shouldBe 2
          }
        }
    }
  }
}
