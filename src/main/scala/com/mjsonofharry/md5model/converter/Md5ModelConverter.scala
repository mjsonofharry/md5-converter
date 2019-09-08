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
    val meshData: String = Source.fromFile(meshSource).getLines.mkString
    val meshParseResult: ParseResult[Md5Mesh] =
      Md5Mesh.parser.parseOnly(meshData)

    val animSource =
      """D:\git\md5-converter\samples\alpha_pinky\animation\cycles\idle1.md5anim"""
    val animDestination =
      """D:\git\md5-converter\output\test.md5anim"""
    val animData: String = Source.fromFile(animSource).getLines.mkString
    val animParseResult: ParseResult[Md5Anim] =
      Md5Anim.parser.parseOnly(animData)

    (meshParseResult, animParseResult) match {
      case (Done(_, md5Mesh: Md5Mesh), Done(_, md5anim: Md5Anim)) => {
        val meshOutput = new PrintWriter(new File(meshDestination))
        meshOutput.write(Md5Mesh.convert(md5Mesh))
        meshOutput.close

        val animOutput = new PrintWriter(new File(animDestination))
        animOutput.write(Md5Anim.convert(md5anim, md5Mesh))
        animOutput.close
      }
      case _ => println(s"Failed to parse mesh or anim")
    }
  }
}
