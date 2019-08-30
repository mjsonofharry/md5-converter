package com.mjsonofharry.md5mesh.converter

import scala.io.Source
import atto._, Atto._
import cats.implicits._
import atto.ParseResult.Done

import com.mjsonofharry.md5mesh.model.Md5Mesh

object Md5MeshConverter {
  def main(args: Array[String]): Unit = {
    val source = args(0)
    val destination = args(1)

    println(s"Reading: '${source}'")
    val data = Source.fromFile(source).getLines.mkString

    val result = Md5Mesh.parser.parseOnly(data)
    result match {
      case Done(input, result) => println("Success!")
      case _ => println(result)
    }
  }
}
