package com.mjsonofharry.md5model.anim

import java.nio.ByteBuffer

import com.mjsonofharry.md5model.mesh.Joint

case class Hierarchy(
  jointName: String,
  parentJointIndex: Int,
  flags: Int,
  startIndex: Int
)

object Hierarchy {
  def apply(joint: Joint, channels: List[Channel], startIndex: Int): Hierarchy = {
    val name = joint.name
    val parentIndex = joint.parentIndex

    val filteredChannels = channels.filter(_.keys.size > 1)

    val attributes = filteredChannels.map(_.attribute).toSet
    val x = attributes.contains("x")
    val y = attributes.contains("y")
    val z = attributes.contains("z")
    val pitch = attributes.contains("pitch")
    val yaw = attributes.contains("yaw")
    val roll = attributes.contains("roll")
    val flags = ByteBuffer
      .wrap(Array(x, y, z, pitch, yaw, roll).map((b: Boolean) => {
        if (b) 1 else 0
      }: Byte))
      .getInt

    val maxRange = filteredChannels.map(_.range._2).max
    val frameValues = List(
      filteredChannels.find(_.attribute == "x"),
      filteredChannels.find(_.attribute == "y"),
      filteredChannels.find(_.attribute == "z"),
      filteredChannels.find(_.attribute == "pitch"),
      filteredChannels.find(_.attribute == "yaw"),
      filteredChannels.find(_.attribute == "roll")
    ).collect { case Some(c) => c }
      .map((c: Channel) => {
        val (start, finish) = c.range
        val prepend = (0 to start).map((n) => c.keys.head).toList
        val append = (0 to maxRange - finish).map((n) => c.keys.last).toList
        prepend ++ c.keys ++ append
      })
      .transpose

    Hierarchy(name, parentIndex, flags, startIndex)
  }
}