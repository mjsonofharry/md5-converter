package com.mjsonofharry.md5model.mesh

import atto._, Atto._
import atto.ParseResult.Done
import cats.implicits._

import com.mjsonofharry.md5model.utils.Utils._
import Vert._
import Tri._
import Weight._

case class Mesh(
    index: Int,
    shader: String,
    numverts: Int,
    numtris: Int,
    numweights: Int,
    verts: List[Vert],
    tris: List[Tri],
    weights: List[Weight]
) {
  def bumpJoints: Mesh =
    this.copy(
      weights = this.weights.map(w => w.copy(jointIndex = w.jointIndex + 1))
    )
}

object Mesh {
  private val root = anyChar ~ string(":/")
  private val doom = string("doom") | string("Doom")
  private val untilTga = manyUntil(anyChar, string(".tga"))
  val minShaderParser: Parser[String] = for {
    path <- root ~ doom ~ string("/base/") ~> untilTga map (_.mkString)
  } yield path
  val shaderNameParser: Parser[String] = for {
    name <- anyChar ~ string(":/") ~ many(manyUntil(anyChar, char('/'))) ~> untilTga map (_.mkString)
  } yield name
  val parser: Parser[Mesh] = for {
    index <- keyValue("mesh", int) <~ char('{') ~ whitespaces
    shader <- keyValue("shader", inQuotes)
    numverts <- keyValue("numverts", int)
    verts <- many(Vert.parser <~ whitespaces) <~ whitespaces
    numtris <- keyValue("numtris", int)
    tris <- many(Tri.parser <~ whitespaces) <~ whitespaces
    numweights <- keyValue("numweights", int)
    weights <- many(Weight.parser <~ whitespaces) <~ whitespaces <~ char('}')
  } yield
    Mesh(index, shader, numverts, numtris, numweights, verts, tris, weights)

  def convert(mesh: Mesh): String = {
    val shaderName = shaderNameParser.parseOnly(mesh.shader) match {
      case Done(_, result: String) => result
      case _                       => ""
    }
    val comment = s"\t// meshes: ${shaderName}\n"
    val minShader = minShaderParser.parseOnly(mesh.shader) match {
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
