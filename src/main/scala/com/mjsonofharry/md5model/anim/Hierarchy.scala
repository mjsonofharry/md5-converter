package com.mjsonofharry.md5model.anim

import com.mjsonofharry.md5model.mesh.Joint
import com.mjsonofharry.md5model.utils.Utils._

case class Hierarchy(
    jointName: JointName,
    parentJointName: JointName,
    parentJointIndex: Int,
    flags: Int,
    attributes: Set[AttributeName],
    startIndex: Int
)

object Hierarchy {
  val powers: Stream[Int] = Stream.from(2).scanLeft(1)((x, _) => x * 2)
  def bin2int(bits: String): Int =
    bits.toList
      .map(_.asDigit)
      .zip(powers.take(bits.size).toList.reverse)
      .map { case(b, exp) => b match {
        case 1 => exp
        case 0 => 0
        case _ =>
          throw new NumberFormatException(s"Cannot convert ${b} into a bit")
      }}
      .reduce(_ + _)

  def apply(
      joint: Joint,
      channels: List[Channel],
      startIndex: Int,
      maxRange: Int
  ): Hierarchy = {
    val name = joint.name
    val parentIndex = joint.parentIndex

    // val filteredChannels: List[Channel] = channels.filter(_.keys.size > 1)

    // val attributes: Set[AttributeName] = filteredChannels.map(_.attribute).toSet
    // val x = attributes.contains("x")
    // val y = attributes.contains("y")
    // val z = attributes.contains("z")
    // val pitch = attributes.contains("pitch")
    // val yaw = attributes.contains("yaw")
    // val roll = attributes.contains("roll")
    // val flags: Int = bin2int(List(x, y, z, pitch, yaw, roll).map(x => x match {
    //   case true => 1
    //   case false => 0
    // }).mkString)

      val flags = bin2int("111111")

    // val transformedAttributes: Set[AttributeName] = attributes.map(_ match {
    //   case "x"     => "Tx"
    //   case "y"     => "Ty"
    //   case "z"     => "Tz"
    //   case "roll"  => "Qx"
    //   case "pitch" => "Qy"
    //   case "yaw"   => "Qz"
    // })
    val transformedAttributes = Set.empty[String]

    Hierarchy(name, joint.parentName, parentIndex, flags, transformedAttributes, startIndex)
  }

  def convert(hierarchy: Hierarchy): String = {
    val attributes =
      if (hierarchy.attributes.size > 0)
        hierarchy.attributes.mkString(start = " (", sep = " ", end = ")")
      else ""
    val comment = s"// ${hierarchy.parentJointName}${attributes}"
    s"${QUOTE}${hierarchy.jointName}${QUOTE}\t${hierarchy.parentJointIndex} ${hierarchy.flags} ${hierarchy.startIndex}\t${comment}"
  }
}
