package com.mjsonofharry.md5model.anim

import atto._, Atto._
import cats.implicits._
import java.nio.ByteBuffer

import com.mjsonofharry.md5model.mesh.{Md5Mesh, Joint}
import com.mjsonofharry.md5model.utils.Utils._
import shapeless.syntax.typeable

case class Md5Anim(
    commandline: String,
    numchannels: Int,
    channels: List[Channel]
)

object Md5Anim {
  val parser: Parser[Md5Anim] = for {
    commandline <- string("MD5Version 6") ~ whitespaces ~ string("commandline") ~> spaceChar ~> inQuotes <~ whitespaces
    numchannels <- string("numchannels") ~ spaceChar ~> int <~ whitespaces
    channels <- many(Channel.parser <~ whitespaces)
  } yield Md5Anim(commandline, numchannels, channels)

  val PITCH = List(0, 1, 0)
  val YAW = List(0, 0, 1)
  val ROLL = List(1, 0, 0)

  val BOUNDS_MIN = "boundsMin"
  val BOUNDS_MAX = "boundsMax"

  type Keys = List[Double]
  type JointName = String
  type Attribute = String

  def convert(md5anim: Md5Anim, md5mesh: Md5Mesh): String = {
    val maxRange: Int = md5anim.channels.map(_.range._2).max
    val jointTable: Map[JointName, Joint] =
      md5mesh.joints.map((j) => (j.name, j)).toMap

    val jointChannels: List[(Joint, List[Channel])] = md5anim.channels
      .filterNot((c) => Set(BOUNDS_MIN, BOUNDS_MAX).contains(c.jointName))
      .groupBy(_.jointName)
      .toList
      .map((g) => (jointTable(g._1), g._2))
      .sortBy(_._1.index)

    val hierarchy: List[Hierarchy] = jointChannels
      .foldLeft((0, List.empty[Hierarchy])) {
        case (
            (i: Int, acc: List[Hierarchy]),
            (joint: Joint, channels: List[Channel])
            ) =>
          (
            i + channels
              .filter(_.keys.size > 1)
              .map(_.attribute)
              .toSet
              .size,
            acc :+ Hierarchy(joint, channels, i)
          )
      }
      ._2

    val boundChannels: List[(JointName, (Attribute, Keys))] = md5anim.channels
      .filter((c) => Set(BOUNDS_MIN, BOUNDS_MAX).contains(c.jointName))
      .map {
        case (channel: Channel) =>
          (
            channel.jointName,
            (channel.attribute, padKeys(channel.keys, channel.range, maxRange))
          )
      }
    val boundsMin = boundChannels.filter(_._1 == BOUNDS_MIN).map(_._2).toMap
    val boundsMax = boundChannels.filter(_._1 == BOUNDS_MAX).map(_._2).toMap
    val bounds = List(
      boundsMin("x"),
      boundsMin("y"),
      boundsMin("z"),
      boundsMax("x"),
      boundsMax("y"),
      boundsMax("z")
    ).transpose

    ""
  }
}
