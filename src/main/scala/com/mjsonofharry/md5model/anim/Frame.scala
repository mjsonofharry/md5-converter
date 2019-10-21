package com.mjsonofharry.md5model.anim

import com.mjsonofharry.md5model.mesh.Joint
import Md5Anim.JointFrameParts
import com.mjsonofharry.md5model.utils.Utils._

case class Frame(index: Int, values: List[FramePart])

object Frame {
  def computeFrames(
      jointFrameParts: List[JointFrameParts]
  ): List[Frame] =
    jointFrameParts
      .map(_._2)
      .transpose
      .zipWithIndex
      .map { case ((parts: List[FramePart], i: Int)) => Frame(i, parts) }

  def computeBaseFrame(
      frames: List[Frame],
      generatedRoot: Option[Joint]
  ): Frame = frames match {
    case head :: _ => {
      val generatedRootFramePart = generatedRoot match {
        case Some(joint) => List(FramePart(joint))
        case None        => Nil: List[FramePart]
      }
      Frame(index = -1, generatedRootFramePart ++ head.values)
    }
    case Nil =>
      throw new AnimConversionException(
        "Could not derive any frames from the animation channels"
      )
  }

  def convert(frame: Frame): String =
    frame.values
      .map(FramePart.convert)
      .filterNot(_.isEmpty)
      .mkString(
        start = s"frame ${frame.index} {\n\t",
        sep = "\n\t",
        end = "\n}"
      )

  def baseConvert(frame: Frame): String =
    frame.values
      .map(FramePart.baseConvert)
      .mkString(start = "baseframe {\n\t", sep = "\n\t", end = "\n}\n")
}
