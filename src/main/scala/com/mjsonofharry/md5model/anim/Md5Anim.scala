package com.mjsonofharry.md5model.anim

import atto._, Atto._
import cats.implicits._

import com.mjsonofharry.md5model.mesh.Md5Mesh
import com.mjsonofharry.md5model.utils.Utils._

case class Md5Anim(
    commandline: String,
    numchannels: Int,
    channels: List[Channel]
)

object Md5Anim {
  val parser: Parser[Md5Anim] = for {
    commandline <- string("MD5Version 6") ~ whitespaces ~ string("commandline") ~> spaceChar ~> inQuotes <~ whitespaces
    numchannels <- string("numchannels") ~ spaceChar ~> int <~ whitespaces
    channels <- many(Channel.parser <~ whitespaces)
  } yield Md5Anim(commandline, numchannels, channels)

  def convert(md5anim: Md5Anim, md5mesh: Md5Mesh) = {
    val jointTable = md5mesh.joints.map((j) => (j.name, j)).toMap

    val joined = md5anim.channels.groupBy(_.joint).map {
      case (j: String, cs: List[Channel]) => (jointTable(j), cs)
    }

    ???
  }
}
