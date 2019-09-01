package com.mjsonofharry.md5model.mesh

import atto._, Atto._
import cats.implicits._

import com.mjsonofharry.md5model.utils.Utils._

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

  def convert(weight: Weight) = {
    val List(x, y, z): List[String] = weight.position.map(format)
    s"weight ${weight.index} ${weight.jointIndex} ${weight.bias} ( ${x} ${y} ${z} )"
  }
}
