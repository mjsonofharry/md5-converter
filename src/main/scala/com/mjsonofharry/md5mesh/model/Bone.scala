package com.mjsonofharry.md5mesh.model

import atto._, Atto._
import cats.implicits._

case class Vector3(
    x: Double,
    y: Double,
    z: Double
)

case class Bone(
    index: Int,
    name: String,
    bindpos: Vector3,
    bindmat: (Vector3, Vector3, Vector3),
    parent: Option[String]
)

object Bone {
  val Parser: Parser[Bone] = for {
    index <- string("bone") ~ spaceChar ~> int <~ spaceChar <~ char('{') ~ many(
      whitespace
    )
    name <- string("name") ~ spaceChar ~ char('"') ~> manyUntil(
      anyChar,
      char('"')
    ) <~ many(whitespace) map (_.mkString)
    x <- string("bindpos") ~ spaceChar ~> double <~ spaceChar
    y <- double <~ spaceChar
    z <- double <~ many(whitespace)
    bindpos = Vector3(x, y, z)
    m00 <- string("bindmat") ~ spaceChar ~> double <~ spaceChar
    m01 <- double <~ spaceChar
    m02 <- double <~ spaceChar
    m10 <- double <~ spaceChar
    m11 <- double <~ spaceChar
    m12 <- double <~ spaceChar
    m20 <- double <~ spaceChar
    m21 <- double <~ spaceChar
    m22 <- double <~ many(whitespace)
    bindmat = (
      Vector3(m00, m01, m02),
      Vector3(m10, m11, m12),
      Vector3(m20, m21, m22)
    )
    parent <- {
      string("parent") ~ spaceChar ~ char('"') ~>
        manyUntil(anyChar, char('"')) <~
        many(whitespace) ~ char('}') map (_.mkString) map (Some(_))
    } | { char('}') >| Option.empty[String] }
  } yield Bone(index, name, bindpos, bindmat, parent)

  def parse(b: String) = Parser parseOnly (b)
}
