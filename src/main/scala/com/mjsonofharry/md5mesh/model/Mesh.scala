package com.mjsonofharry.md5mesh.model

import atto._, Atto._
import cats.implicits._

import Vert._
import Tri._
import Weight._
import ParsingUtils._

case class Mesh(
    index: Int,
    shader: String,
    numverts: Int,
    numtris: Int,
    numweights: Int,
    verts: List[Vert],
    tris: List[Tri],
    weights: List[Weight]
)

object Mesh {
  val shaderParser: Parser[String] = for {
    path <- string("P:/Doom/base/") ~> manyUntil(anyChar, string(".tga")) map (_.mkString)
  } yield path
  val parser: Parser[Mesh] = for {
    index <- string("mesh") ~ spaceChar ~> int <~ spaceChar ~ char('{') ~ whitespaces
    shader <- string("shader") ~ spaceChar ~> inQuotes <~ whitespaces
    numverts <- string("numverts") ~ spaceChar ~> int <~ whitespaces
    verts <- many(Vert.parser <~ whitespaces) <~ whitespaces
    numtris <- string("numtris") ~ spaceChar ~> int <~ whitespaces
    tris <- many(Tri.parser <~ whitespaces) <~ whitespaces
    numweights <- string("numweights") ~ spaceChar ~> int <~ whitespaces
    weights <- many(Weight.parser <~ whitespaces) <~ whitespaces <~ char('}')
  } yield
    Mesh(index, shader, numverts, numtris, numweights, verts, tris, weights)
}
