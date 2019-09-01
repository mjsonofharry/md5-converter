package com.mjsonofharry.md5model.mesh

import org.scalatest._
import atto._, Atto._
import cats.implicits._
import atto.ParseResult.Done

class VertSpec extends FlatSpec with Matchers with GivenWhenThen with Inside {
  val VERT = "	vert 18 0.004956 0.823437 38 2"

  "the vert parser" should "successfully parse a vert" in {
    Given("a vert")
    val vert = VERT

    When("the vert is parsed")
    val result = Vert.parser.parseOnly(vert)

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
