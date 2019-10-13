package com.mjsonofharry.md5model.anim

import com.mjsonofharry.md5model.math.Quaternion
import com.mjsonofharry.md5model.mesh.Joint
import com.mjsonofharry.md5model.utils.Utils._

case class FramePart(
    joint: Joint,
    flags: AttributeFlags,
    x: Double,
    y: Double,
    z: Double,
    orientation: Quaternion
)

object FramePart {
  def apply(joint: Joint): FramePart = FramePart(
    joint = joint,
    flags = AttributeFlags(),
    x = 0.0,
    y = 0.0,
    z = 0.0,
    orientation = Quaternion(0, 0, 0, 0)
  )

  def convert(f: FramePart): String =
    List(
      f.x,
      f.y,
      f.z,
      f.orientation.x,
      f.orientation.y,
      f.orientation.z
    ).zip(f.flags.values).filter(_._2).map(_._1).map(format).mkString(" ")

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
