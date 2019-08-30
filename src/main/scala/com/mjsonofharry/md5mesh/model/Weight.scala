package com.mjsonofharry.md5mesh.model

import atto._, Atto._
import cats.implicits._

case class Weight(
    index: Int,
    jointIndex: Int,
    bias: Double,
    x: Double,
    y: Double,
    z: Double
)

object Weight {
  val parser: Parser[Weight] = for {
    label <- many(whitespace) ~> string("weight") <~ spaceChar
    index <- int <~ spaceChar
    jointIndex <- int <~ spaceChar
    bias <- double <~ spaceChar
    x <- double <~ spaceChar
    y <- double <~ spaceChar
    z <- double
  } yield Weight(index, jointIndex, bias, x, y, z)

  def parse(w: String): ParseResult[Weight] = parser.parseOnly(w)
}
