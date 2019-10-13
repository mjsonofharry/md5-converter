package com.mjsonofharry.md5model.anim

import atto._, Atto._
import cats.implicits._

import com.mjsonofharry.md5model.utils.Utils._

case class Channel(
    index: Int,
    jointName: String,
    attribute: String,
    starttime: Double,
    endtime: Double,
    framerate: Double,
    strings: List[String],
    range: (Int, Int),
    keys: List[Double]
)

object Channel {
  val parser: Parser[Channel] = for {
    index <- keyValue("channel", int) <~ char('{') ~ whitespaces
    jointName <- keyValue("joint", inQuotes)
    attribute <- keyValue("attribute", inQuotes)
    starttime <- keyValue("starttime", double)
    endtime <- keyValue("endtime", double)
    framerate <- keyValue("framerate", double)
    strings <- { keyValue("strings", int) ~> many(inQuotes <~ whitespaces) } | {
      whitespaces >| List("")
    }
    frames = { framerate * endtime }.toInt
    range <- { keyValue("range", { int <~ spaceChar } ~ int) } | {
      whitespaces >| (0, frames)
    }
    keys <- keyValue("keys", int) ~> manyUntil(double <~ whitespaces, char('}'))
  } yield
    Channel(
      index,
      jointName,
      attribute,
      starttime,
      endtime,
      framerate,
      strings,
      range,
      keys
    )

  def apply(
      jointName: String,
      attribute: String,
      framerate: Double,
      frames: Int
  ): Channel = Channel(
    index = -1,
    jointName = jointName,
    attribute = attribute,
    starttime = 0.0,
    endtime = framerate * frames,
    framerate = framerate,
    strings = Nil,
    range = (0, frames - 1),
    keys = {0 until frames}.map(_ => 0.0).toList
  )

  def padKeys(channel: Channel): List[Double] = {
    val keys = channel.keys
    val (start, finish) = channel.range
    val prepended = keys.reverse.padTo(start + keys.size, keys.head)
    val appended = prepended.reverse.padTo(finish, keys.last)
    appended
  }

  def spreadKeys(channel: Channel, total: Int): List[Double] = {
    val keys = channel.keys
    (0 until total)
      .map(x => keys(((x.toDouble / total) * keys.size).toInt))
      .toList
  }
}
