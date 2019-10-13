package com.mjsonofharry.md5model.anim

import com.mjsonofharry.md5model.utils.Utils._

case class AttributeFlags(
    x: Boolean,
    y: Boolean,
    z: Boolean,
    qx: Boolean,
    qy: Boolean,
    qz: Boolean
) {
  def values: List[Boolean] = List(x, y, z, qx, qy, qz)
}

object AttributeFlags {
  def apply(values: List[Boolean]): AttributeFlags = {
    val List(x, y, z, qx, qy, qz) = values
    AttributeFlags(x, y, z, qx, qy, qz)
  }

  def apply(): AttributeFlags = AttributeFlags(false, false, false, false, false, false)
}

case class Frame(index: Int, values: List[FramePart])

object Frame {
  def convert(frame: Frame): String =
    s"frame ${frame.index} " + frame.values
      .map(FramePart.convert)
      .filterNot(_.isEmpty)
      .mkString(start = "{\n\t", sep = "\n\t", end = "\n}")
}
