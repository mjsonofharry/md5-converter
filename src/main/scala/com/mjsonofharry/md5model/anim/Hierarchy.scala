package com.mjsonofharry.md5model.anim

import com.mjsonofharry.md5model.mesh.Joint
import Md5Anim.JointFrameParts
import com.mjsonofharry.md5model.utils.Utils._

case class Hierarchy(
    jointIndex: Int,
    jointName: String,
    parentJointName: String,
    parentJointIndex: Int,
    flags: List[Boolean],
    startIndex: Int
)

object Hierarchy {
  private val powers: Stream[Int] = Stream.from(2).scanLeft(1)((x, _) => x * 2)
  private def bin2int(bits: String): Int =
    bits.toList
      .map(_.asDigit)
      .zip(powers.take(bits.size).toList.reverse)
      .map {
        case (b, exp) =>
          b match {
            case 1 => exp
            case 0 => 0
            case _ =>
              throw new NumberFormatException(s"Cannot convert ${b} into a bit")
          }
      }
      .reduce(_ + _)

  def apply(jointName: String): Hierarchy = Hierarchy(
    jointIndex = 0,
    jointName = jointName,
    parentJointName = "",
    parentJointIndex = -1,
    flags = { 1 to 6 }.map(_ => false).toList,
    startIndex = 0
  )

  def computeHierarchies(
      jointFrameParts: List[JointFrameParts],
      generatedRoot: Option[Joint]
  ): List[Hierarchy] =
    generatedRoot.toList.map(joint => Hierarchy(joint.name)) ++ jointFrameParts
      .foldLeft((0, List.empty[Hierarchy]))((acc, next) => {
        val (i: Int, others: List[Hierarchy]) = acc
        val (joint: Joint, parts: List[FramePart]) = next
        val flags = parts.head.flags.values
        val numAttributes = flags.filter(fl => fl).size
        val startIndex = if (numAttributes > 0) i else 0
        val row =
          Hierarchy(
            joint.index,
            joint.name,
            joint.parentName,
            joint.parentIndex,
            flags,
            startIndex
          )
        (i + numAttributes, others :+ row)
      })
      ._2

  def convert(hierarchy: Hierarchy): String = {
    val attributes =
      List("Tx", "Ty", "Tz", "Qx", "Qy", "Qz")
        .zip(hierarchy.flags)
        .filter(_._2)
        .map(_._1)
    val formattedAttributes =
      if (attributes.size > 0)
        attributes.mkString(start = "( ", sep = " ", end = " )")
      else ""
    val comment = s"// ${hierarchy.parentJointName} ${formattedAttributes}"
    val flags = bin2int(
      hierarchy.flags.map(fl => if (fl) "1" else "0").reduce(_ + _).reverse
    )
    s"${QUOTE}${hierarchy.jointName}${QUOTE}\t${hierarchy.parentJointIndex} ${flags} ${hierarchy.startIndex}\t${comment}"
  }
}
