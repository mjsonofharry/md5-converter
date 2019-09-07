package com.mjsonofharry.md5model.anim

import java.nio.ByteBuffer

import com.mjsonofharry.md5model.mesh.Joint
import com.mjsonofharry.md5model.utils.Utils._

case class Hierarchy(
    jointName: String,
    parentJointIndex: Int,
    flags: Int,
    attributes: Set[String],
    startIndex: Int
)

object Hierarchy {
  def apply(
      joint: Joint,
      channels: List[Channel],
      startIndex: Int
  ): Hierarchy = {
    val name = joint.name
    val parentIndex = joint.parentIndex

    val filteredChannels: List[Channel] = channels.filter(_.keys.size > 1)

    val attributes: Set[String] = filteredChannels.map(_.attribute).toSet
    val x = attributes.contains("x")
    val y = attributes.contains("y")
    val z = attributes.contains("z")
    val pitch = attributes.contains("pitch")
    val yaw = attributes.contains("yaw")
    val roll = attributes.contains("roll")
    val flags: Int = ByteBuffer
      .wrap(Array(x, y, z, pitch, yaw, roll).map((b: Boolean) => {
        if (b) 1 else 0
      }: Byte))
      .getInt

    val transformedAttributes: Set[String] = attributes.map(_ match {
      case "x"     => "Tx"
      case "y"     => "Ty"
      case "z"     => "Tz"
      case "roll"  => "Qx"
      case "pitch" => "Qy"
      case "yaw"   => "Qz"
    })

    val maxRange: Int = filteredChannels.map(_.range._2).max
    val frameValues: List[List[Double]] = List(
      filteredChannels.find(_.attribute == "x"),
      filteredChannels.find(_.attribute == "y"),
      filteredChannels.find(_.attribute == "z"),
      filteredChannels.find(_.attribute == "roll"),
      filteredChannels.find(_.attribute == "pitch"),
      filteredChannels.find(_.attribute == "yaw")
    ).collect { case Some(c) => c }
      .map((c: Channel) => {
        val (start, finish) = c.range
        val prepend = (0 to start).map((n) => c.keys.head).toList
        val append = (0 to maxRange - finish).map((n) => c.keys.last).toList
        prepend ++ c.keys ++ append
      })
      .transpose

    Hierarchy(name, parentIndex, flags, transformedAttributes, startIndex)
  }

  def convert(hierarchy: Hierarchy): String = {
    val comment = s"// ( ${hierarchy.attributes.mkString(" ")} )"
    s"${QUOTE}${hierarchy.jointName}${QUOTE}\t${hierarchy.parentJointIndex} ${hierarchy.flags} ${hierarchy.startIndex}\t${comment}"
  }
}
