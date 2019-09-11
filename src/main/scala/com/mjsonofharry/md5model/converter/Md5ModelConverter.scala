package com.mjsonofharry.md5model.converter

import scala.io.Source
import java.io.{File, PrintWriter}
import atto._, Atto._
import cats.implicits._
import atto.ParseResult.Done

import com.mjsonofharry.md5model.mesh.Md5Mesh
import com.mjsonofharry.md5model.anim.Md5Anim

object Md5MeshConverter {
  def main(args: Array[String]): Unit = {

    val meshSource =
      """D:\git\md5-converter\samples\alpha_pinky\animation\cycles\idle1.md5mesh"""
    val meshDestination =
      """D:\git\md5-converter\output\test.md5mesh"""
    println(s"Reading md5mesh from ${meshSource}")
    val meshData: String = Source.fromFile(meshSource).getLines.mkString
    println("Parsing md5mesh")
    val meshParseResult: ParseResult[Md5Mesh] =
      Md5Mesh.parser.parseOnly(meshData)

    val animSource =
      """D:\git\md5-converter\samples\alpha_pinky\animation\cycles\idle1.md5anim"""
    val animDestination =
      """D:\git\md5-converter\output\test.md5anim"""
    println(s"Reading md5anim from ${animSource}")
    val animData: String = Source.fromFile(animSource).getLines.mkString
    println("Parsing md5anim")
    val animParseResult: ParseResult[Md5Anim] =
      Md5Anim.parser.parseOnly(animData)

    (meshParseResult, animParseResult) match {
      case (Done(_, md5Mesh: Md5Mesh), Done(_, md5anim: Md5Anim)) => {

        println("Converting md5mesh")
        val convertedMesh = Md5Mesh.convert(md5Mesh)
        println(s"Writing converted md5mesh to ${meshDestination}")
        val meshOutput = new PrintWriter(new File(meshDestination))
        meshOutput.write(convertedMesh)
        meshOutput.close

        println("Converting md5anim")
        val convertedAnim = Md5Anim.convert(md5anim, md5Mesh)
        println(s"Writing converted md5anim to ${animDestination}")
        val animOutput = new PrintWriter(new File(animDestination))
        animOutput.write(convertedAnim)
        animOutput.close
      }
      case _ => println(s"Failed to parse mesh or anim")
    }
  }
}
