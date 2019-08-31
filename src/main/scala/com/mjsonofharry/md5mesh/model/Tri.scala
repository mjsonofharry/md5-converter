package com.mjsonofharry.md5mesh.model

import atto._, Atto._
import cats.implicits._

import ParsingUtils._

case class Tri(
    index: Int,
    verts: List[Int]
)

object Tri {
  val parser: Parser[Tri] = for {
    index <- whitespaces ~ string("tri") ~ spaceChar ~> int <~ spaceChar
    v1 <- int <~ spaceChar
    v2 <- int <~ spaceChar
    v3 <- int <~ whitespaces
    verts = List(v1, v2, v3)
  } yield Tri(index, verts)
}
