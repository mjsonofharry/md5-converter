package com.mjsonofharry.md5model.converter

import scala.io.Source
import java.io.{File, PrintWriter}
import java.nio.file.{Paths, NoSuchFileException}
import atto._, Atto._
import cats.implicits._
import atto.ParseResult.Done

import com.mjsonofharry.md5model.mesh.Md5Mesh
import com.mjsonofharry.md5model.anim.Md5Anim
import atto.ParseResult.Fail
import atto.ParseResult.Partial
import com.mjsonofharry.md5model.camera.Md5Camera

final case class ConverterInputException(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

final case class ConversionException(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object Md5MeshConverter {
  def main(args: Array[String]): Unit = {
    val sourcePath: String = Paths.get(args(0)).toAbsolutePath().toString()
    val destinationPath: String = Paths.get(args(1)).toAbsolutePath().toString()

    val source: File = new File(sourcePath)
    if (!source.isDirectory())
      throw new ConverterInputException(s"${sourcePath} is not a directory")
    val sourceFiles: List[String] = Option(source.listFiles()) match {
      case Some(files) =>
        files.filter(_.isFile()).map(_.getAbsolutePath()).toList
      case None => throw new NoSuchFileException(sourcePath)
    }
    val meshPath: String = sourceFiles.filter(_.endsWith(".md5mesh")) match {
      case head :: Nil => head
      case Nil =>
        throw new ConverterInputException(
          s"Expected to find exactly 1 mesh in ${sourcePath} but found none"
        )
      case meshes =>
        throw new ConverterInputException(
          s"Expected to find exactly 1 mesh in ${sourcePath} but found ${meshes.size}"
        )
    }
    val animPaths: List[String] =
      sourceFiles.filter(_.endsWith(".md5anim")).toList

    val destination: File = new File(destinationPath)
    if (!destination.exists()) destination.mkdirs()

    println(s"Mesh:\n  ${meshPath}")
    println(animPaths.mkString(start = "Anims:\n  ", sep = "\n  ", end = ""))
    println(s"Destination:\n  ${destination}")

    val meshName: String = Paths.get(meshPath).getFileName().toString()
    println(s"Converting mesh: ${meshName}")
    val meshDestinationPath: String =
      Paths.get(destinationPath, meshName).toString()
    val meshData: String = Source.fromFile(meshPath).getLines.mkString
    Md5Mesh.parser.parseOnly(meshData) match {
      case Done(_, md5Mesh: Md5Mesh) if md5Mesh.nummeshes > 0 => {
        val convertedMesh: String = Md5Mesh.convert(md5Mesh)
        val meshOutput: PrintWriter = new PrintWriter(
          new File(meshDestinationPath)
        )
        meshOutput.write(convertedMesh)
        meshOutput.close()

        val skipCompression = args.contains("--skip-compression")
        println("Skipping animation compression")

        animPaths.foreach(animPath => {
          val animName: String = Paths.get(animPath).getFileName().toString()
          println(s"Converting anim: ${animName}")
          val animDestinationPath: String =
            Paths.get(destinationPath, animName).toString()
          val animData = Source.fromFile(animPath).getLines.mkString
          Md5Anim.parser.parseOnly(animData) match {
            case Done(_, md5anim: Md5Anim) => {
              val convertedAnim: String =
                Md5Anim.convert(md5anim, md5Mesh, skipCompression)
              val animOutput: PrintWriter =
                new PrintWriter(new File(animDestinationPath))
              animOutput.write(convertedAnim)
              animOutput.close
            }
            case Fail(input, stack, message) =>
              throw new ConversionException(
                s"Animation ${animName} could not be parsed: ${message}"
              )
            case Partial(k) =>
              throw new ConversionException(
                s"Animation ${animName} was only partially parsed (should error not be possible)"
              )
          }
        })
      }
      case Done(_, md5Mesh: Md5Mesh) =>
        animPaths.foreach(animPath => {
          val animName: String = Paths.get(animPath).getFileName().toString()
          println(s"Converting anim (as camera): ${animName}")
          val animData = Source.fromFile(animPath).getLines.mkString
          Md5Anim.parser.parseOnly(animData) match {
            case Done(_, md5anim: Md5Anim) => {
              val cameraExt = ".md5camera"
              val cameraName: String = Paths
                .get(animPath)
                .getFileName()
                .toString()
                .split('.')
                .toList match {
                case head :: _ => head + cameraExt
                case Nil       => animPath + cameraExt
              }
              val cameraDestinationPath: String =
                Paths.get(destinationPath, cameraName).toString()
              val convertedCamera: String = Md5Camera.convert(md5anim, md5Mesh)
              val cameraOutput: PrintWriter =
                new PrintWriter(new File(cameraDestinationPath))
              cameraOutput.write(convertedCamera)
              cameraOutput.close
            }
            case Fail(input, stack, message) =>
              throw new ConversionException(
                s"Animation ${animName} could not be parsed: ${message}"
              )
            case Partial(k) =>
              throw new ConversionException(
                s"Animation ${animName} was only partially parsed (should error not be possible)"
              )
          }
        })
      case Fail(input, stack, message) =>
        ConversionException(s"Mesh ${meshName} could not be parsed: ${message}")
      case Partial(k) =>
        throw new ConversionException(
          s"Mesh ${meshName} was only partially parsed (should error not be possible)"
        )
    }
  }
}
