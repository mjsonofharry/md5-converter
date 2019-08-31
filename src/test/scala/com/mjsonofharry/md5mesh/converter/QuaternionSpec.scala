package com.mjsonofharry.md5mesh.converter

import org.scalatest._

class QuaternionSpec extends FlatSpec with Matchers with GivenWhenThen {
  val P = 0.00000001

  "a matrix" should "be correctly converted to a quaternion" in {
    Given("a 3x3 matrix")
    val m = List(0.0, 1.0, 0.0, -1.0, 0.0, 0.0, 0.0, 0.0, 1.0)

    When("it is converted")
    val q = Quaternion.from_matrix(m).toList

    Then("the correct quaternion components should be returned")
    val Nil :+ x :+ y :+ z :+ w = q
    x shouldEqual(0.0)
    y shouldEqual(0.0)
    z shouldEqual(-0.7071067812 +- P)
  }

  it should "still correctly make more complicated conversions" in {
    Given("a 3x3 matrix")
    val m = List(-0.281509, 0.948261, -0.146812, 0.085640, -0.127560, -0.988127, -0.955729, -0.290740, -0.045299)

    When("it is converted")
    val q = Quaternion.from_matrix(m).toList

    Then("the correct quaternion components should be returned")
    val Nil :+ x :+ y :+ z :+ w = q
    x shouldEqual(0.4720567381 +- P)
    y shouldEqual(0.5475510293 +- P)
    z shouldEqual(-0.5839028446 +- P)
  }
}
