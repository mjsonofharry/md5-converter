package com.mjsonofharry.md5model.mesh

import atto._, Atto._
import cats.implicits._

import com.mjsonofharry.md5model.utils.Utils._

case class Tri(
    index: Int,
    verts: List[Int]
)

object Tri {
  val parser: Parser[Tri] = for {
    index <- whitespaces ~ string("tri") ~ spaceChar ~> int <~ spaceChar
    verts <- vector(3, int)
  } yield Tri(index, verts)

  def convert(tri: Tri): String = {
    val List(v1, v2, v3): List[Int] = tri.verts
    s"tri ${tri.index} ${v1} ${v2} ${v3}"
  }
}
