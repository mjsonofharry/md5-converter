package com.mjsonofharry.md5mesh.model

import atto._, Atto._
import cats.implicits._

import Utils._

case class Md5Mesh(
    commandline: String,
    numbones: Int,
    bones: List[Bone],
    meshes: List[Mesh]
)

object Md5Mesh {
  val parser: Parser[Md5Mesh] = for {
    commandline <- string("MD5Version 6") ~ whitespaces ~ string("commandline") ~> spaceChar ~> inQuotes <~ whitespaces
    numbones <- string("numbones") ~ spaceChar ~> int <~ whitespaces
    bones <- many(Bone.parser <~ whitespaces)
    meshes <- many(Mesh.parser <~ whitespaces)
  } yield Md5Mesh(commandline, numbones, bones, meshes)
}
