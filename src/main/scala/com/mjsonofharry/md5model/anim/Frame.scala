package com.mjsonofharry.md5model.anim

import com.mjsonofharry.md5model.mesh.Joint
import com.mjsonofharry.md5model.math.Quaternion
import com.mjsonofharry.md5model.utils.Utils._

case class AttributeFlags(
    x: Boolean,
    y: Boolean,
    z: Boolean,
    qx: Boolean,
    qy: Boolean,
    qz: Boolean
)

case class FramePart(
    joint: Joint,
    x: Double,
    y: Double,
    z: Double,
    orientation: Quaternion
)

object FramePart {
  def apply(joint: Joint, values: List[Double]): FramePart = {
    val List(x, y, z, yaw, pitch, roll) = values
    val r = Math.PI / 180
    val q = Quaternion.from_euler(yaw * r, pitch * r, roll * r)
    FramePart(joint, x, y, z, q)
  }

  def convert(f: FramePart): String = List(
    f.x,
    f.y,
    f.z,
    f.orientation.x,
    f.orientation.y,
    f.orientation.z
  ).map(format).mkString(" ")

  def baseConvert(f: FramePart): String = {
    val ts = List(
      f.x,
      f.y,
      f.z
    ).map(format).mkString(" ")
    val qs = List(
      f.orientation.x,
      f.orientation.y,
      f.orientation.z
    ).map(format).mkString(" ")
    s"( ${ts} ) ( ${qs} )"
  }
}

case class Frame(index: Int, values: List[FramePart])

object Frame {
  def convert(frame: Frame): String =
    s"frame ${frame.index} " + frame.values
      .map(FramePart.convert)
      .mkString(start = "{\n\t", sep = "\n\t", end = "\n}")
}
