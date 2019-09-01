package com.mjsonofharry.md5model.mesh

import atto._, Atto._
import cats.implicits._

import com.mjsonofharry.md5model.utils.Utils._

case class Md5Mesh(
    commandline: String,
    numbones: Int,
    nummeshes: Int,
    bones: List[Bone],
    meshes: List[Mesh]
)

object Md5Mesh {
  val parser: Parser[Md5Mesh] = for {
    commandline <- string("MD5Version 6") ~ whitespaces ~> keyValue(
      "commandline",
      inQuotes
    )
    numbones <- keyValue("numbones", int)
    bones <- many(Bone.parser <~ whitespaces)
    nummeshes <- keyValue("nummeshes", int)
    meshes <- many(Mesh.parser <~ whitespaces)
  } yield Md5Mesh(commandline, numbones, nummeshes, bones, meshes)

  def convert(md5Mesh: Md5Mesh): String = {
    val version = "MD5Version 10\n"
    val commandline = s"commandline ${quotate(md5Mesh.commandline)}\n\n"
    val numJoints = s"numJoints ${md5Mesh.numbones}\n"
    val numMeshes = s"numMeshes ${md5Mesh.nummeshes}\n\n"
    val boneTable = md5Mesh.bones.map((b: Bone) => (b.name, b.index)).toMap
    val joints = md5Mesh.bones
      .map(Bone.convert(_, boneTable))
      .mkString(start = "joints {\n\t", sep = "\n\t", end = "\n}\n\n")
    val meshes = md5Mesh.meshes.map(Mesh.convert).mkString("\n")

    version + commandline + numJoints + numMeshes + joints + meshes
  }
}
