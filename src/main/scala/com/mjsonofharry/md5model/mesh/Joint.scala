package com.mjsonofharry.md5model.mesh

import com.mjsonofharry.md5model.math.Quaternion
import com.mjsonofharry.md5model.utils.Utils._

case class Joint(
    index: Int,
    name: String,
    position: List[Double],
    orientation: Quaternion,
    parentIndex: Int,
    parentName: String
)

object Joint {
  def apply(bone: Bone, boneTable: Map[String, Int]): Joint = {
    val orientation = Quaternion.from_matrix(bone.bindmat)
    val parentName = bone.parent.getOrElse("")
    val parentIndex = boneTable.getOrElse(parentName, -1)
    Joint(
      bone.index,
      bone.name,
      bone.bindpos,
      orientation,
      parentIndex,
      parentName
    )
  }

  def convert(joint: Joint): String = {
    val position = joint.position
      .map(format)
      .mkString(start = "( ", sep = " ", end = " )")
    val orientation =
      List(joint.orientation.x, joint.orientation.y, joint.orientation.z)
        .map(format)
        .mkString(start = "( ", sep = " ", end = " )")
    s"${QUOTE}${joint.name}${QUOTE}\t${joint.parentIndex} ${position} ${orientation}\t\t// ${joint.parentName}"
  }
}
