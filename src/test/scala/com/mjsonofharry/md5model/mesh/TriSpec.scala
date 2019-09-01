package com.mjsonofharry.md5model.mesh

import org.scalatest._
import atto._, Atto._
import cats.implicits._
import atto.ParseResult.Done

class TriSpec extends FlatSpec with Matchers with GivenWhenThen with Inside {
  val TRI = "	tri 45 25 38 44"

  "the tri parser" should "successfully parse a tri" in {
    Given("a tri")
    val tri = TRI

    When("the tri is parsed")
    val result = Tri.parser.parseOnly(tri)

    Then("a tri object should be constructed")
    result should matchPattern { case Done(_: String, _: Tri) => }

    inside(result) {
      case Done(_: String, tri: Tri) =>
        inside(tri) {
          case Tri(index: Int, verts: List[Int]) => {
            index shouldBe 45
            verts should contain theSameElementsInOrderAs List(25, 38, 44)
          }
        }
    }
  }
}
