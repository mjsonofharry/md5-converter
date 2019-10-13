package com.mjsonofharry.md5model.mesh

import atto._, Atto._
import cats.implicits._

import com.mjsonofharry.md5model.utils.Utils._

final case class MeshConversionException(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

case class Md5Mesh(
    commandline: String,
    numbones: Int,
    nummeshes: Int,
    joints: List[Joint],
    meshes: List[Mesh]
)

object Md5Mesh {
  val ORIGIN_GENERATED = "origin_GENERATED"

  val parser: Parser[Md5Mesh] = for {
    commandline <- string("MD5Version 6") ~ whitespaces ~> keyValue(
      "commandline",
      inQuotes
    )
    numbones <- keyValue("numbones", int)
    unsafeBones <- many(Bone.parser <~ whitespaces)
    bones = unsafeBones.filter(_.parent.isEmpty) match {
      case _ :: _ :: _ => {
        Bone(ORIGIN_GENERATED) +: unsafeBones.map(_.reroot(ORIGIN_GENERATED))
      }
      case _ :: Nil => unsafeBones
      case Nil      => throw new MeshConversionException("Mesh has no root bone")
    }
    boneTable = bones.map((b: Bone) => (b.name, b.index)).toMap
    joints = bones.map(Joint(_, boneTable))
    nummeshes <- keyValue("nummeshes", int)
    meshes <- many(Mesh.parser <~ whitespaces) map (
        ms => if (bones.size == unsafeBones.size) ms else ms.map(_.bumpJoints)
    )
  } yield Md5Mesh(commandline, numbones, nummeshes, joints, meshes)

  def convert(md5Mesh: Md5Mesh): String = {
    val version = "MD5Version 10"
    val commandline = s"commandline ${quotate(md5Mesh.commandline)}\n"
    val numJoints = s"numJoints ${md5Mesh.joints.size}"
    val numMeshes = s"numMeshes ${md5Mesh.nummeshes}\n"
    val jointBlock = md5Mesh.joints
      .map(Joint.convert)
      .mkString(start = "joints {\n\t", sep = "\n\t", end = "\n}\n")
    val meshBlocks = md5Mesh.meshes.map(Mesh.convert).mkString("\n")

    List(version, commandline, numJoints, numMeshes, jointBlock, meshBlocks)
      .mkString("\n")
  }
}
