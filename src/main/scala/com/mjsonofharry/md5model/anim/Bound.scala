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
  val MIN = "boundsMin"
  val MAX = "boundsMax"

  def apply(values: List[Double]): Bound = {
    val List(minX, minY, minZ, maxX, maxY, maxZ) = values
    Bound(minX, minY, minZ, maxX, maxY, maxZ)
  }

  def convert(bound: Bound): String =
    s"( ${bound.minX} ${bound.minY} ${bound.minZ} ) ( ${bound.maxX} ${bound.maxY} ${bound.maxZ} )"
}
