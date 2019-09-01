package com.mjsonofharry.md5model.converter

import scala.io.Source
import java.io.{File, PrintWriter}
import atto._, Atto._
import cats.implicits._
import atto.ParseResult.Done

import com.mjsonofharry.md5model.mesh.Md5Mesh

object Md5MeshConverter {
  def main(args: Array[String]): Unit = {
    val source = args(0)
    val destination = args(1)

    val data: String = Source.fromFile(source).getLines.mkString

    val parseResult: ParseResult[Md5Mesh] = Md5Mesh.parser.parseOnly(data)

    parseResult match {
      case Done(_, md5Mesh: Md5Mesh) => {
        val output = new PrintWriter(new File(destination))
        output.write(Md5Mesh.convert(md5Mesh))
        output.close
      }
      case _ => println(s"Failed to parse md5mesh read from '${source}'")
    }
  }
}
