package com.mjsonofharry.md5model.anim

import org.scalatest._

class BoundSpec extends FlatSpec with Matchers with GivenWhenThen with Inside {
  "the bounds" should "be computed correctly for animations that have bounds channels" in {
    Given("a list of channels including bounds")
    val frameCount = 240
    val channels: List[Channel] = for {
      (jointName, n) <- List(Bound.MIN, Bound.MAX, "someJoint")
        .zip(List(-1.0, 1.0, 0.0))
      attribute <- List("x", "y", "z", "pitch", "yaw", "roll")
    } yield
      Channel(jointName, attribute, 24.0, frameCount).copy(keys = {
        0 until frameCount
      }.map(_ => n).toList)

    When("bounds are computed from the channels")
    val bounds: List[Bound] = Bound.computeBounds(channels)

    Then("the bounds should be correctly represented")
    bounds.size shouldBe frameCount
    bounds
      .flatMap(b => List(b.maxX, b.maxY, b.maxZ))
      .distinct should contain theSameElementsAs List(1.0)
    bounds
      .flatMap(b => List(b.minX, b.minY, b.minZ))
      .distinct should contain theSameElementsAs List(-1.0)
  }

  it should "be empty for animations that do not have bounds channels" in {
    Given("a list of channels that does not include bounds")
    val frameCount = 240
    val channels: List[Channel] = for {
      attribute <- List("x", "y", "z", "pitch", "yaw", "roll")
    } yield Channel("aJoint", attribute, 24.0, 240)

    When("bounds are computed from the channels")
    val bounds: List[Bound] = Bound.computeBounds(channels)

    Then("an empty list should be returned")
    bounds should be (Nil)
  }
}
