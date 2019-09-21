package com.mjsonofharry.md5model.anim

import com.mjsonofharry.md5model.mesh.Joint
import com.mjsonofharry.md5model.math.Quaternion
import com.mjsonofharry.md5model.utils.Utils._

case class AttributeFlags(x: Boolean, y: Boolean, z: Boolean, qx: Boolean, qy: Boolean, qz: Boolean)

case class FramePart(
    joint: Joint,
    x: Double,
    y: Double,
    z: Double,
    orientation: Quaternion,
)

case class Frame(index: Int, values: List[FramePart])

object Frame {
  def convert(frame: Frame): String =
    s"frame ${frame.index} " + frame.values
      .map(
        f =>
          s"${f.x} ${f.y} ${f.z} ${format(f.orientation.x)} ${format(f.orientation.y)} ${format(f.orientation.z)}"
      )
      .mkString(start = "{\n\t", sep = "\n\t", end = "\n}")
}
