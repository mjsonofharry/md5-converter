package com.mjsonofharry.md5mesh.model

import atto._, Atto._
import atto.ParseResult.Done
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
  val shaderNameParser: Parser[String] = for {
    name <- string("P:/") ~ many(manyUntil(anyChar, char('/'))) ~> manyUntil(
      anyChar,
      string(".tga")
    ) map (_.mkString)
  } yield name
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

  def convert(mesh: Mesh): String = {
    val shaderName = shaderNameParser.parseOnly(mesh.shader) match {
      case Done(_, result: String) => result
      case _                       => ""
    }
    val comment = s"\t// meshes: ${shaderName}\n"
    val minShader = shaderParser.parseOnly(mesh.shader) match {
      case Done(_, result: String) => result
      case _                       => ""
    }
    val shader = s"\tshader ${quotate(minShader)}\n\n"
    val verts = mesh.verts
      .map(Vert.convert)
      .mkString(
        start = s"\tnumverts ${mesh.numverts}\n\t",
        sep = "\n\t",
        end = "\n\n"
      )
    val tris = mesh.tris
      .map(Tri.convert)
      .mkString(
        start = s"\tnumtris ${mesh.numtris}\n\t",
        sep = "\n\t",
        end = "\n\n"
      )
    val weights = mesh.weights
      .map(Weight.convert)
      .mkString(
        start = s"\tnumweights ${mesh.numweights}\n\t",
        sep = "\n\t",
        end = "\n"
      )

    "mesh {\n" + comment + shader + verts + tris + weights + "}\n"
  }
}
