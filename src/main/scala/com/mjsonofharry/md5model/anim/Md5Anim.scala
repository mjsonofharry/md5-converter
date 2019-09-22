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

  def convert(md5anim: Md5Anim, md5mesh: Md5Mesh): String = {
    val fps: Int = md5anim.channels.head.framerate.toInt
    val frameCount: Int = md5anim.channels.map(_.endtime).max.toInt * fps
    val jointTable: Map[String, Joint] =
      md5mesh.joints.map((j) => (j.name, j)).toMap
    val jointChannels: List[(Joint, List[Channel])] = md5anim.channels
      .filterNot(c => Set(Bound.MIN, Bound.MAX).contains(c.jointName))
      .groupBy(_.jointName)
      .toList
      .map((g) => (jointTable(g._1), g._2))
      .sortBy(_._1.index)

    val frames: List[Frame] = jointChannels
      .map {
        case (joint: Joint, channels: List[Channel]) => {
          val attributeKeys: Map[String, List[Double]] = channels
            .map(c => (c.attribute, Channel.padKeys(c, frameCount)))
            .toMap
          List(
            attributeKeys("x"),
            attributeKeys("y"),
            attributeKeys("z"),
            attributeKeys("yaw"),
            attributeKeys("pitch"),
            attributeKeys("roll")
          ).transpose.map(FramePart(joint, _))
        }
      }
      .transpose
      .zipWithIndex
      .map { case ((parts: List[FramePart], i: Int)) => Frame(i, parts) }

    val hierarchy: List[Hierarchy] = jointChannels
      .foldLeft((0, List.empty[Hierarchy]))((acc, next) => {
        val (i: Int, others: List[Hierarchy]) = acc
        val (joint: Joint, channels: List[Channel]) = next
        val numAttributes = 6
        val flags = List(true, true, true, true, true, true)
        val row =
          Hierarchy(joint.name, joint.parentName, joint.parentIndex, flags, i)
        (i + numAttributes, others :+ row)
      })
      ._2

    val boundChannels: List[(String, (String, List[Double]))] =
      md5anim.channels
        .filter(c => Set(Bound.MIN, Bound.MAX).contains(c.jointName))
        .map(c => (c.jointName, (c.attribute, Channel.padKeys(c, frameCount))))
    val boundsMin = boundChannels.filter(_._1 == Bound.MIN).map(_._2).toMap
    val boundsMax = boundChannels.filter(_._1 == Bound.MAX).map(_._2).toMap
    val bounds: List[Bound] = List(
      boundsMin("x"),
      boundsMin("y"),
      boundsMin("z"),
      boundsMax("x"),
      boundsMax("y"),
      boundsMax("z")
    ).transpose.map(Bound(_))

    val version = "MD5Version 10\n"
    val commandline = s"commandline ${quotate(md5anim.commandline)}\n\n"
    val firstChannel = md5anim.channels.head
    val numFrames = s"numFrames ${frameCount}\n"
    val numJoints = s"numJoints ${md5mesh.joints.size}\n"
    val frameRate = s"frameRate ${fps}\n"
    // val numAnimatedComponents =
    //   s"numAnimatedComponents ${hierarchy.flatMap(_.attributes).size}\n\n"
    val numAnimatedComponents =
      s"numAnimatedComponents ${hierarchy.size * 6}\n\n"

    val hierarchyBlock: String = hierarchy
      .map(Hierarchy.convert)
      .mkString(start = "hierarchy {\n\t", sep = "\n\t", end = "\n}\n\n")

    val boundsBlock: String = bounds
      .map(Bound.convert)
      .mkString(start = "bounds {\n\t", sep = "\n\t", end = "\n}\n\n")

    val baseFrameBlock: String = frames.head.values
      .map(FramePart.baseConvert)
      .mkString(start = "baseframe {\n\t", sep = "\n\t", end = "\n}\n\n")

    val frameBlocks: String = frames
      .map(Frame.convert)
      .mkString(start = "\n", sep = "\n\n", end = "\n")

    version + commandline + numFrames + numJoints + frameRate + numAnimatedComponents + hierarchyBlock + boundsBlock + baseFrameBlock + frameBlocks
  }
}
