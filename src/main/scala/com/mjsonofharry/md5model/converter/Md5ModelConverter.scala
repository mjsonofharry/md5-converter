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
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object Md5MeshConverter {
  def main(args: Array[String]): Unit = {
    val cwd: String = Paths.get("").toAbsolutePath().toString()
    val sourcePath: String = Paths.get(cwd, args(0)).toString()
    val destinationPath: String = Paths.get(cwd, args(1)).toString()

    val source: File = new File(sourcePath)
    val sourceFiles: List[String] = Option(source.listFiles()) match {
      case Some(files) =>
        files.filter(_.isFile()).map(_.getAbsolutePath()).toList
      case None => throw new NoSuchFileException(sourcePath)
    }
    val meshPath: String = sourceFiles.find(_.endsWith(".md5mesh")) match {
      case Some(path) => path
      case None       => throw new NoMeshFoundException(sourcePath)
    }
    val animPaths: List[String] =
      sourceFiles.filter(_.endsWith(".md5anim")).toList

    val destination: File = new File(destinationPath)
    if (!destination.exists()) destination.mkdirs()

    println(s"Mesh:\n  ${meshPath}")
    println(
      s"Anims:\n${animPaths.mkString(start = "  ", sep = "\n  ", end = "")}"
    )
    println(s"Destination:\n  ${destination}")

    val meshName: String = Paths.get(meshPath).getFileName().toString()
    println(s"Converting mesh: ${meshName}")
    val meshDestinationPath: String =
      Paths.get(destinationPath, meshName).toString()
    val meshData: String = Source.fromFile(meshPath).getLines.mkString
    Md5Mesh.parser.parseOnly(meshData) match {
      case Done(_, md5Mesh: Md5Mesh) => {
        val convertedMesh: String = Md5Mesh.convert(md5Mesh)
        val meshOutput: PrintWriter = new PrintWriter(
          new File(meshDestinationPath)
        )
        meshOutput.write(convertedMesh)
        meshOutput.close()

        animPaths.foreach(animPath => {
          val animName: String = Paths.get(animPath).getFileName().toString()
          println(s"Converting anim: ${animName}")
          val animDestinationPath: String =
            Paths.get(destinationPath, animName).toString()
          val animData = Source.fromFile(animPath).getLines.mkString
          Md5Anim.parser.parseOnly(animData) match {
            case Done(_, md5anim: Md5Anim) => {
              val convertedAnim: String = Md5Anim.convert(md5anim, md5Mesh)
              val animOutput: PrintWriter =
                new PrintWriter(new File(animDestinationPath))
              animOutput.write(convertedAnim)
              animOutput.close
            }
          }
        })
      }
    }
  }
}
