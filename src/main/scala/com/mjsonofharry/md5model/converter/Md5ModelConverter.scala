package com.mjsonofharry.md5model.converter

import scala.io.Source
import java.io.{File, PrintWriter}
import java.nio.file.{Paths, NoSuchFileException}
import atto._, Atto._
import cats.implicits._
import atto.ParseResult.Done

import com.mjsonofharry.md5model.mesh.Md5Mesh
import com.mjsonofharry.md5model.anim.Md5Anim

final case class NoMeshFoundException(
    private val message: String = "", 
    private val cause: Throwable = None.orNull)
extends Exception(message, cause) 

object Md5MeshConverter {
  def main(args: Array[String]): Unit = {
    val cwd = Paths.get("").toAbsolutePath().toString()
    val sourcePath = Paths.get(cwd, args(0)).toString()
    val destinationPath = Paths.get(cwd, args(1)).toString()

    val source = new File(sourcePath)
    val sourceFiles = Option(source.listFiles()) match {
      case Some(files) => files.filter(_.isFile()).map(_.getAbsolutePath())
      case None => throw new NoSuchFileException(sourcePath)
    }
    val meshPath = sourceFiles.find(_.endsWith(".md5mesh")) match {
      case Some(path) => path
      case None => throw new NoMeshFoundException(sourcePath)
    }
    val animPaths = sourceFiles.filter(_.endsWith(".md5anim")).toList

    val destination = new File(destinationPath)
    if (!destination.exists()) destination.mkdirs()

    println(s"Mesh:\n  ${meshPath}")
    println(s"Anims:\n${animPaths.mkString(start="  ", sep="\n  ", end="")}")
    println(s"Destination:\n  ${destination}")

    val meshName = Paths.get(meshPath).getFileName().toString()
    println(s"Converting ${meshName}")
    val meshDestinationPath = Paths.get(destinationPath, meshName).toString()
    Md5Mesh.parser.parseOnly(Source.fromFile(meshPath).getLines.mkString) match {
      case Done(_, md5Mesh) => {
        val convertedMesh = Md5Mesh.convert(md5Mesh)
        val meshOutput = new PrintWriter(new File(meshDestinationPath))
        meshOutput.write(convertedMesh)
        meshOutput.close()

        animPaths.foreach(animPath => {
          val animName = Paths.get(animPath).getFileName().toString()
          println(s"Converting ${animName}")
          val animDestinationPath = Paths.get(destinationPath, animName).toString()
          Md5Anim.parser.parseOnly(Source.fromFile(animPath).getLines.mkString) match {
            case Done(_, md5anim) => {
              val convertedAnim = Md5Anim.convert(md5anim, md5Mesh)
              val animOutput = new PrintWriter(new File(animDestinationPath))
              animOutput.write(convertedAnim)
              animOutput.close
            }
          }
        })
      }
    }
  }
}
