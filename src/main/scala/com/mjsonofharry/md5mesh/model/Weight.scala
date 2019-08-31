package com.mjsonofharry.md5mesh.model

import atto._, Atto._
import cats.implicits._

import ParsingUtils._

case class Weight(
    index: Int,
    jointIndex: Int,
    bias: Double,
    position: List[Double]
)

object Weight {
  val parser: Parser[Weight] = for {
    index <- whitespaces ~ string("weight") ~ spaceChar ~> int <~ spaceChar
    jointIndex <- int <~ spaceChar
    bias <- double <~ spaceChar
    x <- double <~ spaceChar
    y <- double <~ spaceChar
    z <- double
    position = List(x, y, z)
  } yield Weight(index, jointIndex, bias, position)
}
