package com.mjsonofharry.md5model.anim

import com.mjsonofharry.md5model.mesh.Joint
import com.mjsonofharry.md5model.utils.Utils._

case class Hierarchy(
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
