package com.mjsonofharry.md5model.anim

import org.scalatest._
import com.mjsonofharry.md5model.math.Quaternion
import com.mjsonofharry.md5model.mesh.Joint

class FrameSpec extends FlatSpec with Matchers with GivenWhenThen with Inside {
  val JOINT_1 = Joint(
      index = 0,
      name = "origin",
      position = List(0.0, 0.0, 0.0),
      orientation = Quaternion(0, 0, 0, 0),
      parentIndex = -1,
      parentName = "",
      generated = true
    )

    val JOINT_2 = JOINT_1.copy(
      index = 1,
      name = "joint",
      parentIndex = JOINT_1.index,
      parentName = JOINT_1.name
    )

  "a frame" should "be correctly computed from frameparts" in {
    

    val parts = List()
  }
}
