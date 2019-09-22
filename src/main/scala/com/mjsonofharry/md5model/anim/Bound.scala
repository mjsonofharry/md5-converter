package com.mjsonofharry.md5model.anim
import com.mjsonofharry.md5model.utils.Utils._

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

  def convert(bound: Bound): String = {
    val minBounds = List(
      bound.minX,
      bound.minX,
      bound.minZ
    ).map(format).mkString(" ")
    val maxBounds = List(
      bound.maxX,
      bound.maxY,
      bound.maxZ
    ).map(format).mkString(" ")
    s"( ${minBounds} ) ( ${maxBounds} )"
  }
}
