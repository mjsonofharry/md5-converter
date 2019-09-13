package com.mjsonofharry.md5model.anim

import com.mjsonofharry.md5model.mesh.Joint

case class FramePart(
    joint: Joint,
    x: Double,
    y: Double,
    z: Double,
    qx: Double,
    qy: Double,
    qz: Double,
    qw: Double
) {
  def values = List(x, y, z, qx, qy, qz)
}

case class Frame(index: Int, values: List[FramePart])

object Frame {
  def convert(frame: Frame): String =
    s"frame ${frame.index} " + frame.values
      .map(f => s"${f.x} ${f.y} ${f.z} ${f.qx} ${f.qy} ${f.qz}")
      .mkString(start = "{\n\t", sep = "\n\t", end = "\n}")
}
