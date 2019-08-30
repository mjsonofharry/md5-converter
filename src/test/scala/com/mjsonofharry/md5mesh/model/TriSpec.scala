package com.mjsonofharry.md5mesh.model

import org.scalatest._
import atto.ParseResult.Done

class TriSpec extends FlatSpec with Matchers with GivenWhenThen with Inside {
  val tri = "	tri 45 25 38 44"

  "the tri parser" should "successfully parse a tri" in {
    Given("a tri")
    val t = tri

    When("the tri is parsed")
    val result = Tri.parse(t)

    Then("a tri object should be constructed")
    result should matchPattern { case Done(_: String, _: Tri) => }

    inside(result) {
      case Done(_: String, tri: Tri) =>
        inside(tri) {
          case Tri(index: Int, verts: (Int, Int, Int)) => {
            index shouldBe 45
            verts shouldBe (25, 38, 44)
          }
        }
    }
  }
}
