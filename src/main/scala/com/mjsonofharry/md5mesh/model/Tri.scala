package com.mjsonofharry.md5mesh.model

import atto._, Atto._
import cats.implicits._

case class Tri(
    index: Int,
    verts: (Int, Int, Int)
)

object Tri {
  val parser: Parser[Tri] = for {
    label <- many(whitespace) ~> string("tri") <~ spaceChar
    index <- int <~ spaceChar
    v1 <- int <~ spaceChar
    v2 <- int <~ spaceChar
    v3 <- int <~ many(whitespace)
  } yield Tri(index, (v1, v2, v3))

  def parse(t: String): ParseResult[Tri] = parser.parseOnly(t)
}
