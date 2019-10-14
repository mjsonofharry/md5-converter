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

  def computeBounds(channels: List[Channel]): List[Bound] = {
    val boundChannels: List[(String, (String, List[Double]))] =
      channels
        .filter(c => Set(Bound.MIN, Bound.MAX).contains(c.jointName))
        .map(c => (c.jointName, (c.attribute, Channel.padKeys(c))))
    val boundsMin: Map[String, List[Double]] =
      boundChannels.filter(_._1 == Bound.MIN).map(_._2).toMap
    val boundsMax: Map[String, List[Double]] =
      boundChannels.filter(_._1 == Bound.MAX).map(_._2).toMap
    val bounds: Option[List[Bound]] = for {
      minX <- boundsMin.get("x")
      minY <- boundsMin.get("y")
      minZ <- boundsMin.get("z")
      maxX <- boundsMax.get("x")
      maxY <- boundsMax.get("y")
      maxZ <- boundsMax.get("z")
    } yield List(minX, minY, minZ, maxX, maxY, maxZ).transpose.map(Bound(_))
    bounds.getOrElse(Nil)
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
