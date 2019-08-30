package com.mjsonofharry.md5mesh.model

import atto._, Atto._
import cats.implicits._
import Utils._

case class Bone(
    index: Int,
    name: String,
    bindpos: Vector3,
    bindmat: (Vector3, Vector3, Vector3),
    parent: Option[String]
)

object Bone {
  val parser: Parser[Bone] = for {
    index <- string("bone") ~ spaceChar ~> int <~ spaceChar <~ char('{') ~ whitespaces
    name <- string("name") ~ spaceChar ~> inQuotes <~ whitespaces map (_.mkString)
    x <- string("bindpos") ~ spaceChar ~> double <~ spaceChar
    y <- double <~ spaceChar
    z <- double <~ whitespaces
    bindpos = Vector3(x, y, z)
    m00 <- string("bindmat") ~ spaceChar ~> double <~ spaceChar
    m01 <- double <~ spaceChar
    m02 <- double <~ spaceChar
    m10 <- double <~ spaceChar
    m11 <- double <~ spaceChar
    m12 <- double <~ spaceChar
    m20 <- double <~ spaceChar
    m21 <- double <~ spaceChar
    m22 <- double <~ whitespaces
    bindmat = (
      Vector3(m00, m01, m02),
      Vector3(m10, m11, m12),
      Vector3(m20, m21, m22)
    )
    parent <- {
      string("parent") ~ spaceChar ~> inQuotes <~ whitespaces ~ char('}') map (Some(
        _
      ))
    } | { char('}') >| Option.empty[String] }
  } yield Bone(index, name, bindpos, bindmat, parent)

  def parse(b: String): ParseResult[Bone] = parser parseOnly (b)
}
