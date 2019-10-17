package com.mjsonofharry.md5model.camera

import com.mjsonofharry.md5model.utils.Utils._
import com.mjsonofharry.md5model.math.Quaternion
import com.mjsonofharry.md5model.anim.FramePart
import com.mjsonofharry.md5model.mesh.Joint

case class CameraPart(
    camera: Joint,
    x: Double,
    y: Double,
    z: Double,
    orientation: Quaternion,
    fov: Double
)

object CameraPart {
  def apply(framePart: FramePart, fov: Double): CameraPart = CameraPart(
    camera = framePart.joint,
    x = framePart.x,
    y = framePart.y,
    z = framePart.z,
    orientation = framePart.orientation,
    fov = fov
  )

  def convert(camera: CameraPart): String = {
    val position =
      List(camera.x, camera.y, camera.z)
        .map(format)
        .mkString(" ")
    val orientation =
      List(camera.orientation.x, camera.orientation.y, camera.orientation.z)
        .map(format)
        .mkString(" ")
    val fov = format(camera.fov)

    s"( ${position} ) ( ${orientation} ) ${fov}"
  }
}
