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

  val PITCH = List(0, 1, 0)
  val YAW = List(0, 0, 1)
  val ROLL = List(1, 0, 0)

  val BOUNDS_MIN: JointName = "boundsMin"
  val BOUNDS_MAX: JointName = "boundsMax"

  def mapJointsToChannels(
      channels: List[Channel],
      joints: List[Joint]
  ): List[(Joint, List[Channel])] = {
    val jointTable: Map[String, Joint] = joints.map((j) => (j.name, j)).toMap
    channels
      .filterNot(c => Set(BOUNDS_MIN, BOUNDS_MAX).contains(c.jointName))
      .groupBy(_.jointName)
      .toList
      .map((g) => (jointTable(g._1), g._2))
      .sortBy(_._1.index)
  }

  def constructHierarchy(
      jointChannels: List[(Joint, List[Channel])],
      maxRange: Int
  ): List[Hierarchy] =
    jointChannels
      .foldLeft((0, List.empty[Hierarchy]))((acc, next) => {
        val (i: Int, others: List[Hierarchy]) = acc
        val (joint: Joint, channels: List[Channel]) = next
        val numAttributes = channels
          .filter(_.keys.size > 1)
          .map(_.attribute)
          .toSet
          .size
        (i + numAttributes, others :+ Hierarchy(joint, channels, i, maxRange))
      })
      ._2

  def computeBounds(channels: List[Channel], maxRange: Int) = {
    val boundChannels: List[(JointName, (AttributeName, List[Key]))] = channels
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
    List(
      boundsMin("x"),
      boundsMin("y"),
      boundsMin("z"),
      boundsMax("x"),
      boundsMax("y"),
      boundsMax("z")
    ).transpose.map(Bound(_))
  }

  def computeFrames(
      jointChannels: List[(Joint, List[Channel])],
      maxRange: Int
  ): List[Frame] = {
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
                val q = Quaternion.from_euler(yaw, pitch, roll)
                FramePart(
                  joint = joint,
                  x = x,
                  y = y,
                  z = z,
                  qx = q.x,
                  qy = q.y,
                  qz = q.z,
                  qw = q.w
                )
              }
            }

        (joint, keys)
      }
    }

    val frames = { 0 to maxRange - 1 }.toList
      .map(frameIndex => {
        Frame(frameIndex, jointValues.map {
          case (joint: Joint, frameParts: List[FramePart]) => {
            frameParts.get(frameIndex).get
          }
        })
      })
    frames
  }

  def computeBaseFrame(frames: List[Frame]): List[FramePart] = frames.head.values

  def convert(md5anim: Md5Anim, md5mesh: Md5Mesh): String = {
    val maxRange: Int = md5anim.channels.map(_.range._2).max
    val jointChannels: List[(Joint, List[Channel])] =
      mapJointsToChannels(md5anim.channels, md5mesh.joints)
    val hierarchy: List[Hierarchy] = constructHierarchy(jointChannels, maxRange)
    val bounds: List[Bound] = computeBounds(md5anim.channels, maxRange)
    val frames: List[Frame] = computeFrames(jointChannels, maxRange)
    val baseFrame: List[FramePart] = computeBaseFrame(frames)
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
      .map(f => s"( ${f.x} ${f.y} ${f.z} ) ( ${f.qx} ${f.qy} ${f.qz} )")
      .mkString(start = "baseframe {\n\t", sep = "\n\t", end = "\n}\n\n")

    val convertedFrames = frames
      .map(Frame.convert)
      .mkString(start = "\n", sep = "\n\n", end = "\n")

    version + commandline + numFrames + numJoints + frameRate + numAnimatedComponents + convertedHierarchy + convertedBounds + convertedBaseFrame + convertedFrames
  }
}
