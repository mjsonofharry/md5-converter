package com.mjsonofharry.md5model.camera

import com.mjsonofharry.md5model.mesh.Md5Mesh
import com.mjsonofharry.md5model.anim.{Md5Anim, Channel}
import Md5Anim.JointFrameParts
import com.mjsonofharry.md5model.utils.Utils._

final case class CameraConversionException(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

case class Md5Camera(
    commandline: String,
    numFrames: Int,
    frameRate: Int,
    numCuts: Int,
    cuts: List[Int],
    camera: List[CameraPart]
)

object Md5Camera {
  val CAMERA_1 = "camera1"
  val REF = "refcam"

  def convert(md5anim: Md5Anim, md5mesh: Md5Mesh): String = {
    val fps: Int = md5anim.channels.head.framerate.toInt
    val frameCount: Int = { md5anim.channels.map(_.endtime).max * fps }.toInt
    val cameraNames: List[String] =
      md5anim.channels.find(_.jointName == REF) match {
        case Some(refcam) => {
          val cameraNames = {
            CAMERA_1 +: refcam.strings.filter(
              md5mesh.joints.map(_.name).contains
            )
          }.distinct
          Channel.padKeys(refcam).map(r => cameraNames(r.toInt))
        }
        case None => { 0 until frameCount }.toList.map(_ => CAMERA_1)
      }
    val cuts =
      cameraNames.zipWithIndex
        .groupBy(_._1)
        .map(_._2.head)
        .toList
        .map(_._2)
        .sorted
        .tail
    val cameraFov: Map[String, Double] = md5anim.channels
      .filter(_.attribute == "fov")
      .map(c => c.jointName -> c.keys.head)
      .toMap
    val cameraFrameParts: List[JointFrameParts] =
      Md5Anim.mapJointsToFrameParts(md5mesh, md5anim)
    val cameraParts: Map[String, List[CameraPart]] = {
      for {
        (camera, frameParts) <- cameraFrameParts
        framePart <- frameParts
        fov <- cameraFov.get(camera.name)
      } yield CameraPart(framePart, fov)
    }.groupBy(_.camera.name).toMap
    val cameras: List[CameraPart] = cameraNames.zipWithIndex.map {
      case (cameraName, frameIndex) => cameraParts(cameraName)(frameIndex)
    }

    val version = "MD5Version 10"
    val commandline = s"commandline ${quotate(md5mesh.commandline)}\n"
    val numFrames = s"numFrames ${frameCount}"
    val frameRate = s"frameRate ${fps}"
    val numCuts = s"numCuts ${cuts.size}\n"
    val cutsBlock =
      if (cuts.size > 0)
        cuts.mkString(start = "cuts {\n\t", sep = "\n\t", end = "\n}\n")
      else "cuts {\n}\n"
    val cameraBlock = cameras
      .map(CameraPart.convert)
      .mkString(start = "camera {\n\t", sep = "\n\t", end = "\n}\n")

    List(
      version,
      commandline,
      numFrames,
      frameRate,
      numCuts,
      cutsBlock,
      cameraBlock
    ).mkString("\n")
  }
}
