package com.mjsonofharry.md5model.anim

case class Bound(
    minX: Double,
    minY: Double,
    minZ: Double,
    maxX: Double,
    maxY: Double,
    maxZ: Double
)

object Bound {
  def apply(values: List[Double]): Bound = {
    val List(minX, minY, minZ, maxX, maxY, maxZ) = values
    Bound(minX, minY, minZ, maxX, maxY, maxZ)
  }
}