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
    strings: Int,
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
    strings <- keyValue("strings", int)
    range <- keyValue("range", { int <~ spaceChar } ~ int)
    numkeys <- keyValue("keys", int)
    keys <- manyUntil(double <~ whitespaces, char('}'))
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

  def padKeys(channel: Channel, padTo: Int): List[Double] = {
    val keys = channel.keys
    val (start, finish) = channel.range
    val prependTo = start + keys.size
    val appendTo = finish
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
