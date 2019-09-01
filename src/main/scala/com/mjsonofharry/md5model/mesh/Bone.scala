package com.mjsonofharry.md5model.mesh

import atto._, Atto._
import cats.implicits._

import com.mjsonofharry.md5model.math.Quaternion
import com.mjsonofharry.md5model.utils.Utils._

case class Bone(
    index: Int,
    name: String,
    bindpos: List[Double],
    bindmat: List[Double],
    parent: Option[String]
)

object Bone {
  val parser: Parser[Bone] = for {
    index <- keyValue("bone", int) <~ char('{') ~ whitespaces
    name <- keyValue("name", inQuotes) map (_.mkString)
    bindpos <- keyValue("bindpos", vector(3, double))
    bindmat <- keyValue("bindmat", vector(9, double))
    parent <- { keyValue("parent", inQuotes) <~ char('}') map (Some(_)) } | {
      char('}') >| Option.empty[String]
    }
  } yield Bone(index, name, bindpos, bindmat, parent)

  def convert(bone: Bone, boneTable: Map[String, Int]): String = {
    val q: List[Double] = Quaternion.from_matrix(bone.bindmat)
    val List(px, py, pz): List[String] = bone.bindpos.map(format)
    val List(qx, qy, qz, qw): List[String] = q.map(format)

    val position = s"( ${px} ${py} ${pz} )"
    val orientation = s"( ${qx} ${qy} ${qz} )"
    val parentName = bone.parent.getOrElse("")
    val parentIndex = boneTable.getOrElse(parentName, -1)
    s"${QUOTE}${bone.name}${QUOTE}\t${parentIndex} ${position} ${orientation}\t\t// ${parentName}"
  }
}
