package com.mjsonofharry.md5mesh.model

import atto._, Atto._
import cats.implicits._

import ParsingUtils._

case class Bone(
    index: Int,
    name: String,
    bindpos: List[Double],
    bindmat: List[Double],
    parent: Option[String]
)

object Bone {
  val parser: Parser[Bone] = for {
    index <- string("bone") ~ spaceChar ~> int <~ spaceChar <~ char('{') ~ whitespaces
    name <- string("name") ~ spaceChar ~> inQuotes <~ whitespaces map (_.mkString)
    x <- string("bindpos") ~ spaceChar ~> double <~ spaceChar
    y <- double <~ spaceChar
    z <- double <~ whitespaces
    bindpos = List(x, y, z)
    m00 <- string("bindmat") ~ spaceChar ~> double <~ spaceChar
    m01 <- double <~ spaceChar
    m02 <- double <~ spaceChar
    m10 <- double <~ spaceChar
    m11 <- double <~ spaceChar
    m12 <- double <~ spaceChar
    m20 <- double <~ spaceChar
    m21 <- double <~ spaceChar
    m22 <- double <~ whitespaces
    bindmat = List(m00, m01, m02, m10, m11, m12, m20, m21, m22)
    parent <- {
      string("parent") ~ spaceChar ~> inQuotes <~ whitespaces ~ char('}') map (Some(
        _
      ))
    } | { char('}') >| Option.empty[String] }
  } yield Bone(index, name, bindpos, bindmat, parent)
}
