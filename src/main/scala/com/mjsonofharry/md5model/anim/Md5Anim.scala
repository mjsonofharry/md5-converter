package com.mjsonofharry.md5model.anim

import atto._, Atto._
import cats.implicits._

import com.mjsonofharry.md5model.mesh.{Md5Mesh, Joint}
import com.mjsonofharry.md5model.utils.Utils._
import com.mjsonofharry.md5model.math.Quaternion

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

  val BOUNDS_MIN: JointName = "boundsMin"
  val BOUNDS_MAX: JointName = "boundsMax"

  def convert(md5anim: Md5Anim, md5mesh: Md5Mesh): String = {
    val maxRange: Int = md5anim.channels.map(_.range._2).max
    val jointTable: Map[String, Joint] =
      md5mesh.joints.map((j) => (j.name, j)).toMap
    val jointChannels: List[(Joint, List[Channel])] = md5anim.channels
      .filterNot(c => Set(BOUNDS_MIN, BOUNDS_MAX).contains(c.jointName))
      .groupBy(_.jointName)
      .toList
      .map((g) => (jointTable(g._1), g._2))
      .sortBy(_._1.index)

    val hierarchy: List[Hierarchy] = jointChannels
      .foldLeft((0, List.empty[Hierarchy]))((acc, next) => {
        val (i: Int, others: List[Hierarchy]) = acc
        val (joint: Joint, channels: List[Channel]) = next
        val numAttributes = channels
          .map(_.attribute)
          .toSet
          .size
        (i + numAttributes, others :+ Hierarchy(joint, channels, i, maxRange))
      })
      ._2

    val boundChannels: List[(JointName, (AttributeName, List[Double]))] =
      md5anim.channels
        .filter(c => Set(BOUNDS_MIN, BOUNDS_MAX).contains(c.jointName))
        .map(
          c =>
            (
              c.jointName: JointName,
              (c.attribute: AttributeName, padKeys(c.keys, c.range, maxRange))
            )
        )
    val boundsMin = boundChannels.filter(_._1 == BOUNDS_MIN).map(_._2).toMap
    val boundsMax = boundChannels.filter(_._1 == BOUNDS_MAX).map(_._2).toMap
    val bounds: List[Bound] = List(
      boundsMin("x"),
      boundsMin("y"),
      boundsMin("z"),
      boundsMax("x"),
      boundsMax("y"),
      boundsMax("z")
    ).transpose.map(Bound(_))

    val jointValues: List[(Joint, List[FramePart])] = jointChannels.map {
      case (joint: Joint, channels: List[Channel]) => {
        val channelMap = channels.map(c => (c.attribute, c)).toMap
        val attributeKeys: Map[String, List[Double]] = channels
          .map(c => (c.attribute, padKeys(c.keys, c.range, maxRange)))
          .toMap
        val xKeys: List[Double] = attributeKeys("x")
        val yKeys: List[Double] = attributeKeys("y")
        val zKeys: List[Double] = attributeKeys("z")
        val yawKeys: List[Double] = attributeKeys("yaw")
        val pitchKeys: List[Double] = attributeKeys("pitch")
        val rollKeys: List[Double] = attributeKeys("roll")
        val keys: List[FramePart] =
          List(xKeys, yKeys, zKeys, yawKeys, pitchKeys, rollKeys).transpose
            .map {
              case List(x, y, z, yaw, pitch, roll) => {
                val r = Math.PI / 180
                val orientation =
                  Quaternion.from_euler5(yaw * r, pitch * r, roll * r)
                FramePart(joint, x, y, z, orientation)
              }
            }

        (joint, keys)
      }
    }
    val frames: List[Frame] = { 0 until maxRange }.toList
      .map(frameIndex => {
        Frame(frameIndex, jointValues.map {
          case (joint: Joint, frameParts: List[FramePart]) => {
            frameParts.get(frameIndex).get
          }
        })
      })

    val baseFrame: List[FramePart] = frames.head.values

    val firstChannel = md5anim.channels.head

    val version = "MD5Version 10\n"
    val commandline = s"commandline ${quotate(md5anim.commandline)}\n\n"
    val numFrames =
      s"numFrames ${(firstChannel.framerate * firstChannel.framerate).toInt}\n"
    val numJoints = s"numJoints ${md5mesh.joints.size}\n"
    val frameRate = s"frameRate ${firstChannel.framerate}\n"
    // val numAnimatedComponents =
    //   s"numAnimatedComponents ${hierarchy.flatMap(_.attributes).size}\n\n"
    val numAnimatedComponents =
      s"numAnimatedComponents ${hierarchy.size * 6}\n\n"

    val convertedHierarchy = hierarchy
      .map(Hierarchy.convert)
      .mkString(start = "hierarchy {\n\t", sep = "\n\t", end = "\n}\n\n")

    val convertedBounds = bounds
      .map(Bound.convert)
      .mkString(start = "bounds {\n\t", sep = "\n\t", end = "\n}\n\n")

    val convertedBaseFrame = baseFrame
      .map(
        f =>
          s"( ${f.x} ${f.y} ${f.z} ) ( ${format(f.orientation.x)} ${format(
            f.orientation.y
          )} ${format(f.orientation.z)} )"
      )
      .mkString(start = "baseframe {\n\t", sep = "\n\t", end = "\n}\n\n")

    val convertedFrames = frames
      .map(Frame.convert)
      .mkString(start = "\n", sep = "\n\n", end = "\n")

    version + commandline + numFrames + numJoints + frameRate + numAnimatedComponents + convertedHierarchy + convertedBounds + convertedBaseFrame + convertedFrames
  }
}
