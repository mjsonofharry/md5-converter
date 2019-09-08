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

  type Channels = List[Channel]
  type Keys = List[Double]
  type JointName = String
  type Attribute = String

  def mapJointsToChannels(
      channels: List[Channel],
      joints: List[Joint]
  ): List[(Joint, Channels)] = {
    val jointTable: Map[JointName, Joint] = joints.map((j) => (j.name, j)).toMap
    channels
      .filterNot(c => Set(BOUNDS_MIN, BOUNDS_MAX).contains(c.jointName))
      .groupBy(_.jointName)
      .toList
      .map((g) => (jointTable(g._1), g._2))
      .sortBy(_._1.index)
  }

  def constructHierarchy(
      jointChannels: List[(Joint, Channels)]
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
        (i + numAttributes, others :+ Hierarchy(joint, channels, i))
      })
      ._2

  def computeBounds(jointChannels: List[(Joint, Channels)]) = {
    val channels = jointChannels.flatMap(_._2)
    val maxRange: Int = channels.map(_.range._2).max
    val boundChannels: List[(JointName, (Attribute, Keys))] = channels
      .filter(c => Set(BOUNDS_MIN, BOUNDS_MAX).contains(c.jointName))
      .map(
        c => (c.jointName, (c.attribute, padKeys(c.keys, c.range, maxRange)))
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
    ).transpose.transpose.map(Bound(_))
  }

  def computeFrames(jointChannels: List[(Joint, Channels)]): List[Frame] =
    jointChannels
      .map(_._2.filter(_.keys.size > 1).map(_.keys))
      .transpose
      .zipWithIndex
      .map { case (values: List[List[Double]], i: Int) => Frame(i, values) }

  def convert(md5anim: Md5Anim, md5mesh: Md5Mesh): String = {
    val jointChannels: List[(Joint, Channels)] =
      mapJointsToChannels(md5anim.channels, md5mesh.joints)

    val hierarchy: List[Hierarchy] = constructHierarchy(jointChannels)

    val bounds: List[Bound] = computeBounds(jointChannels)

    val baseFrame = Nil

    val frames = computeFrames(jointChannels)

    val version = "MD5Version 10\n"
    val commandline = s"commandline ${quotate(md5anim.commandline)}\n\n"
    val numFrames = s"numFrames ${frames.size}\n"
    val numJoints = s"numJoints ${md5mesh.joints.size}\n"
    val frameRate = s"frameRate ${md5anim.channels.head.framerate}\n"
    val numAnimatedComponents =
      s"numAnimatedComponents ${hierarchy.flatMap(_.attributes).size}\n"

    val convertedHierarchy = hierarchy
      .map(Hierarchy.convert)
      .mkString(start = "hierarchy {\n\t", sep = "\n\t", end = "\n}\n\n")

    val convertedBounds = bounds
      .map(Bound.convert)
      .mkString(start = "bounds {\n\t", sep = "\n\t", end = "\n}\n\n")

    val convertedBaseFrame = "baseframe {}\n\n"

    val convertedFrames = frames
      .map(Frame.convert)
      .mkString(start = "\n", sep = "\n\n", end = "\n")

    version + commandline + numFrames + numJoints + frameRate + numAnimatedComponents + convertedHierarchy + convertedBaseFrame + convertedFrames
  }
}
