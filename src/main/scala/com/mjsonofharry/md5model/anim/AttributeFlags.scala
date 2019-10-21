package com.mjsonofharry.md5model.anim

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

  def apply(value: Boolean = false): AttributeFlags =
    AttributeFlags(value, value, value, value, value, value)
}
