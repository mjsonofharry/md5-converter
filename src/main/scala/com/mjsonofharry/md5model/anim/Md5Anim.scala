package com.mjsonofharry.md5model.anim

import atto._, Atto._
import cats.implicits._

import com.mjsonofharry.md5model.mesh.{Md5Mesh, Joint}
import com.mjsonofharry.md5model.utils.Utils._
import com.mjsonofharry.md5model.math.Quaternion

final case class AnimConversionException(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

case class Md5Anim(
    commandline: String,
    numchannels: Int,
    channels: List[Channel]
)

object Md5Anim {
  type JointFrameParts = (Joint, List[FramePart])

  val parser: Parser[Md5Anim] = for {
    commandline <- string("MD5Version 6") ~ whitespaces ~ string("commandline") ~> spaceChar ~> inQuotes <~ whitespaces
    numchannels <- string("numchannels") ~ spaceChar ~> int <~ whitespaces
    channels <- many(Channel.parser <~ whitespaces)
  } yield Md5Anim(commandline, numchannels, channels)

  def mapJointsToFrameParts(
      md5mesh: Md5Mesh,
      md5anim: Md5Anim,
      skipCompression: Boolean = false
  ): List[JointFrameParts] = {
    val jointNameLookup: Map[String, Joint] =
      md5mesh.joints.map(j => (j.name, j)).toMap
    md5anim.channels
      .filterNot(c => Set(Bound.MIN, Bound.MAX).contains(c.jointName))
      .groupBy(_.jointName)
      .toList
      .map(g => (jointNameLookup.get(g._1), g._2))
      .collect { case ((Some(joint), channels)) => (joint, channels) }
      .sortBy(_._1.index)
      .map {
        case (joint: Joint, channels: List[Channel]) => {
          val attributes: Map[String, List[Double]] = channels
            .map(c => (c.attribute, Channel.padKeys(c)))
            .toMap
          val values: List[List[Double]] = List(
            attributes("x"),
            attributes("y"),
            attributes("z"),
            attributes("yaw"),
            attributes("pitch"),
            attributes("roll")
          ).transpose.map(components => {
            val List(x, y, z, yaw, pitch, roll) = components
            val r = Math.PI / 180
            val q = Quaternion.from_euler(yaw * r, pitch * r, roll * r)
            List(x, y, z, q.x, q.y, q.z, q.w)
          })
          val flags: AttributeFlags =
            if (skipCompression) AttributeFlags(true)
            else
              AttributeFlags(values.transpose.take(6).map(_.distinct.size > 1))
          val parts: List[FramePart] = values.map(components => {
            val List(x, y, z, qx, qy, qz, qw) = components
            val q = Quaternion(qw, qx, qy, qz)
            FramePart(joint, flags, x, y, z, q)
          })
          (joint, parts)
        }
      }
  }

  def convert(
      md5anim: Md5Anim,
      md5mesh: Md5Mesh,
      skipCompression: Boolean
  ): String = {
    val fps: Int = md5anim.channels.head.framerate.toInt
    val frameCount: Int = { md5anim.channels.map(_.range._2).max }

    val jointFrameParts: List[JointFrameParts] =
      mapJointsToFrameParts(md5mesh, md5anim, skipCompression)
    val frames: List[Frame] = Frame.computeFrames(jointFrameParts)
    val generatedRoot: Option[Joint] = md5mesh.joints.find(_.generated)
    val hierarchy: List[Hierarchy] =
      Hierarchy.computeHierarchies(jointFrameParts, generatedRoot)
    val bounds = Bound.computeBounds(md5anim.channels)
    val baseframe: Frame = Frame.computeBaseFrame(frames, generatedRoot)

    val version = "MD5Version 10"
    val commandline = s"commandline ${quotate(md5anim.commandline)}\n"
    val firstChannel = md5anim.channels.head
    val numFrames = s"numFrames ${frameCount}"
    val numJoints = s"numJoints ${md5mesh.joints.size}"
    val frameRate = s"frameRate ${fps}"
    val numAnimatedComponents =
      s"numAnimatedComponents ${hierarchy.flatMap(_.flags).filter(fl => fl).size}\n"
    val hierarchyBlock: String = hierarchy
      .map(Hierarchy.convert)
      .mkString(start = "hierarchy {\n\t", sep = "\n\t", end = "\n}\n")
    val boundsBlock: String = bounds
      .map(Bound.convert)
      .mkString(start = "bounds {\n\t", sep = "\n\t", end = "\n}\n")
    val baseFrameBlock: String = Frame.baseConvert(baseframe)
    val frameBlocks: String = frames.map(Frame.convert).mkString("\n\n")

    List(
      version,
      commandline,
      numFrames,
      numJoints,
      frameRate,
      numAnimatedComponents,
      hierarchyBlock,
      boundsBlock,
      baseFrameBlock,
      frameBlocks
    ).mkString("\n")
  }
}
