package com.mjsonofharry.md5model.anim

import atto._, Atto._
import cats.implicits._
import java.nio.ByteBuffer

import com.mjsonofharry.md5model.mesh.{Md5Mesh, Joint}
import com.mjsonofharry.md5model.utils.Utils._

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

  def convert(md5anim: Md5Anim, md5mesh: Md5Mesh) = {
    val joints: Map[String, Joint] =
      md5mesh.joints.map((j) => (j.name, j)).toMap

    val jointChannels: List[(Joint, List[Channel])] = md5anim.channels
      .groupBy(_.jointName)
      .map {
        case (jointName: String, channels: List[Channel]) =>
          (joints(jointName), channels.filter(_.jointName == jointName))
      }
      .toList
      .sortBy(_._1.index)

    val jointStartIndices: Map[String, Int] = jointChannels.toList
      .foldLeft((0, Nil: List[(String, Int)])) {
        case (
            (currentIndex: Int, acc: List[(String, Int)]),
            (joint: Joint, channels: List[Channel])
            ) =>
          (
            currentIndex + channels
              .filter(_.keys.size > 1)
              .map(_.attribute)
              .toSet
              .size,
            acc :+ (joint.name, currentIndex)
          )
      }
      ._2
      .toMap

    val jointHierarchies: List[Hierarchy] = jointChannels.map {
      case (joint: Joint, channels: List[Channel]) =>
        Hierarchy(joint, channels, jointStartIndices(joint.name))
    }
  }
}
