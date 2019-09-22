package com.mjsonofharry.md5model.anim

import com.mjsonofharry.md5model.utils.Utils._

case class AttributeFlags(
    x: Boolean,
    y: Boolean,
    z: Boolean,
    qx: Boolean,
    qy: Boolean,
    qz: Boolean
)

case class Frame(index: Int, values: List[FramePart])

object Frame {
  def convert(frame: Frame): String =
    s"frame ${frame.index} " + frame.values
      .map(FramePart.convert)
      .mkString(start = "{\n\t", sep = "\n\t", end = "\n}")
}
