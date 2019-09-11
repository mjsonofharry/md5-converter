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

  val BOUNDS_MIN: JointName = "boundsMin"
  val BOUNDS_MAX: JointName = "boundsMax"

  def mapJointsToChannels(
      channels: List[Channel],
      joints: List[Joint]
  ): List[(Joint, List[Channel])] = {
    val jointTable: Map[JointName, Joint] = joints.map((j) => (j.name, j)).toMap
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
    val jointValues = jointChannels.map {
      case (joint: Joint, channels: List[Channel]) => {
        val filterChannels = channels.filter(_.keys.size > 1)
        val values = List(
          filterChannels.find(_.attribute == "x"),
          filterChannels.find(_.attribute == "y"),
          filterChannels.find(_.attribute == "z"),
          filterChannels.find(_.attribute == "roll"),
          filterChannels.find(_.attribute == "pitch"),
          filterChannels.find(_.attribute == "yaw")
        ).collect { case Some(c) => padKeys(c.keys, c.range, maxRange) }
        (joint, values.transpose)
      }
    }

    { 0 to maxRange - 1 }.toList
      .map(frameIndex => {
        println(s"  Generating frame ${frameIndex}")
        Frame(frameIndex, jointValues.map(jv => {
          println(s"    Fetching value for ${jv._1.name}")
          jv._2.get(frameIndex).getOrElse(Nil: List[Key])
        }))
      })
  }

  def convert(md5anim: Md5Anim, md5mesh: Md5Mesh): String = {
    val maxRange: Int = md5anim.channels.map(_.range._2).max
    val frameCount: Int = md5anim.channels.map(c => {c.framerate * c.endtime}.toInt).max
    assert(maxRange == frameCount)
    println(s"Counted ${frameCount} frames")

    println("Mapping joints to channels")
    val jointChannels: List[(Joint, List[Channel])] =
      mapJointsToChannels(md5anim.channels, md5mesh.joints)
    println(s"Mapped ${jointChannels.size} joints to their channels")

    println("Creating hierarchy...")
    val hierarchy: List[Hierarchy] = constructHierarchy(jointChannels, maxRange)
    println(s"Hierarchy contains ${hierarchy.size} joints")
    hierarchy.foreach(h => println(s"  ${h.jointName}"))

    println("Creating bounds...")
    val bounds: List[Bound] = computeBounds(md5anim.channels, maxRange)
    println(s"Created ${bounds.size} bounds")

    println("Creating baseframe...")
    val baseFrame = Nil
    println("Skipped for now")

    println("Creating frames...")
    val frames = computeFrames(jointChannels, maxRange)
    println(s"Created ${frames.size} frames")

    val firstChannel = md5anim.channels.head

    val version = "MD5Version 10\n"
    val commandline = s"commandline ${quotate(md5anim.commandline)}\n\n"
    val numFrames =
      s"numFrames ${firstChannel.framerate * firstChannel.framerate}\n"
    val numJoints = s"numJoints ${md5mesh.joints.size}\n"
    val frameRate = s"frameRate ${firstChannel.framerate}\n"
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
